import importlib.util
import contextlib
import ctypes as C
import io
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    "keychain_wallet", Path(__file__).parents[1] / "tools/keychain_wallet.py"
)
wallet = importlib.util.module_from_spec(spec)
try:
    spec.loader.exec_module(wallet)
except ModuleNotFoundError as error:
    if (error.name or "").split(".")[0] not in {"cryptography", "Crypto", "eth_account"}:
        raise
    raise unittest.SkipTest("key helper crypto module missing: " + error.name) from error

FIXTURE = bytes.fromhex("00" * 31 + "01")
PUBLIC = "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf"


class Store:
    def __init__(self, fail=False):
        self.calls = []
        self.fail = fail

    def add(self, service, account, label, secret):
        self.calls.append((service, account, label, bytes(secret)))
        if self.fail:
            raise RuntimeError("key store refused")


class KeychainWalletTests(unittest.TestCase):
    def test_missing_private_settings_reports_reason(self):
        with tempfile.TemporaryDirectory() as temporary:
            error = io.StringIO()
            store = Store()
            with patch.object(wallet.Path, "home", return_value=Path(temporary)), \
                    patch.object(wallet, "MacKeychain", return_value=store), \
                    patch.object(wallet, "generate") as generate, \
                    patch.object(sys, "argv", ["setup", "--create", "agent"]), \
                    contextlib.redirect_stderr(error):
                self.assertEqual(wallet.main(), 1)
            self.assertIn("inspect the named item", error.getvalue())
            self.assertEqual(json.loads(error.getvalue())["reason"],
                             "private Desk settings directory required")
            generate.assert_not_called()
            self.assertEqual(store.calls, [])

    def test_unexpected_errors_keep_fixed_message(self):
        for failure in (ValueError(FIXTURE.hex()), RuntimeError(FIXTURE.hex())):
            with self.subTest(error=type(failure).__name__), tempfile.TemporaryDirectory() as temporary:
                error = io.StringIO()
                with patch.object(wallet.Path, "home", return_value=Path(temporary)), \
                        patch.object(wallet, "generate") as generate, \
                        patch.object(wallet, "MacKeychain", side_effect=failure), \
                        patch.object(sys, "argv", ["setup", "--create", "agent"]), \
                        contextlib.redirect_stderr(error):
                    self.assertEqual(wallet.main(), 1)
                self.assertEqual(json.loads(error.getvalue())["message"],
                                 "Setup did not finish. Preserve any setup record and inspect the named item before retrying.")
                self.assertNotIn("reason", json.loads(error.getvalue()))
                self.assertNotIn(FIXTURE.hex(), error.getvalue())
                generate.assert_not_called()

    def test_setup_failure_after_add_keeps_recovery_instruction_and_reason(self):
        reason = "new item did not refuse access without a prompt"

        class RefusedStore(Store):
            def add(self, *args):
                super().add(*args)
                raise wallet.SetupError(reason)

        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            directory = home / ".config/desk"
            directory.mkdir(parents=True, mode=0o700)
            store = RefusedStore()
            error = io.StringIO()
            with patch.object(wallet.Path, "home", return_value=home), \
                    patch.object(wallet, "MacKeychain", return_value=store), \
                    patch.object(wallet, "generate", return_value=bytearray(FIXTURE)), \
                    patch.object(sys, "argv", ["setup", "--create", "agent"]), \
                    contextlib.redirect_stderr(error):
                self.assertEqual(wallet.main(), 1)
            self.assertEqual(len(store.calls), 1)
            stopped = json.loads(error.getvalue())
            self.assertIn("inspect the named item", stopped["message"])
            self.assertEqual(stopped["reason"], reason)
            self.assertEqual(stopped["item"]["account"], PUBLIC)

    def test_missing_module_without_name_preserves_original_error(self):
        failure = ModuleNotFoundError("unnamed module")
        with patch.object(type(spec.loader), "exec_module", side_effect=failure):
            with self.assertRaises(ModuleNotFoundError) as raised:
                exec(compile(Path(__file__).read_text(), __file__, "exec"),
                     {"__file__": __file__})
        self.assertIs(raised.exception, failure)

    def test_known_address(self):
        self.assertEqual(wallet.address(FIXTURE), PUBLIC)

    def test_reject_invalid_secrets(self):
        for value in (b"", bytes(31), bytes(32), bytes([255]) * 32, bytes(33)):
            with self.subTest(value_length=len(value)):
                with self.assertRaises(wallet.SetupError):
                    wallet.address(value)

    def test_owner_and_trading_roles_use_separate_services(self):
        for role, service in (("owner", "desk-owner"), ("agent", "desk")):
            with self.subTest(role=role):
                store = Store()
                result = wallet.create(role, store, lambda: bytearray(FIXTURE))
                self.assertEqual(len(store.calls), 1)
                actual = store.calls[0]
                self.assertEqual(actual[:2], (service, PUBLIC))
                self.assertEqual(actual[3], FIXTURE.hex().encode("ascii"))
                self.assertEqual(result, {"role": role, "service": service, "account": PUBLIC})

    def test_refusal_does_not_report_success(self):
        store = Store(fail=True)
        secret = bytearray(FIXTURE)
        with self.assertRaisesRegex(RuntimeError, "key store refused"):
            wallet.create("agent", store, lambda: secret)
        self.assertEqual(len(store.calls), 1)
        self.assertEqual(secret, bytes(32))

    def test_success_wipes_generated_buffer(self):
        secret = bytearray(FIXTURE)
        wallet.create("owner", Store(), lambda: secret)
        self.assertEqual(secret, bytes(32))

    def test_invalid_role_never_generates_or_stores(self):
        calls = []
        store = Store()
        with self.assertRaises(wallet.SetupError):
            wallet.create("unknown", store, lambda: calls.append(True))
        self.assertEqual(calls, [])
        self.assertEqual(store.calls, [])

    def test_noninteractive_probe_accepts_only_access_refusal(self):
        self.assertTrue(wallet.acceptable_read_status(-25308))
        for status in (0, -25300, -25293, -50):
            with self.subTest(status=status):
                self.assertFalse(wallet.acceptable_read_status(status))

    def test_independent_address_disagreement_refuses_storage(self):
        class Wrong:
            address = "0x" + "00" * 20
        with patch.object(wallet.Account, "from_key", return_value=Wrong()):
            with self.assertRaisesRegex(wallet.SetupError, "independent public addresses differ"):
                wallet.address(FIXTURE)

    def test_public_record_precedes_storage_and_keeps_both_states(self):
        for fail in (False, True):
            with self.subTest(fail=fail), tempfile.TemporaryDirectory() as temporary:
                home = Path(temporary)
                directory = home / ".config/desk"
                directory.mkdir(parents=True, mode=0o700)
                path = directory / "keychain-agent.jsonl"
                captured = []

                class ObservedStore(Store):
                    def add(self, *args):
                        captured.extend(json.loads(line) for line in path.read_text().splitlines())
                        super().add(*args)

                output, error = io.StringIO(), io.StringIO()
                with patch.object(wallet.Path, "home", return_value=home), \
                        patch.object(wallet, "MacKeychain", return_value=ObservedStore(fail)), \
                        patch.object(wallet, "generate", return_value=bytearray(FIXTURE)), \
                        patch.object(sys, "argv", ["setup", "--create", "agent"]), \
                        contextlib.redirect_stdout(output), contextlib.redirect_stderr(error):
                    result = wallet.main()
                self.assertEqual(result, 1 if fail else 0)
                expected = {"role": "agent", "service": "desk", "account": PUBLIC}
                self.assertEqual(captured, [{**expected, "state": "prepared"}])
                records = [json.loads(line) for line in path.read_text().splitlines()]
                self.assertEqual(records[0], captured[0])
                self.assertEqual(len(records), 1 if fail else 2)
                if not fail:
                    self.assertEqual(records[1], {**expected, "state": "stored", "prompt_required": True})
                    self.assertEqual(json.loads(output.getvalue()), records[1])
                self.assertEqual(path.stat().st_mode & 0o777, 0o600)
                self.assertNotIn(FIXTURE.hex(), output.getvalue() + error.getvalue() + path.read_text())

    def test_existing_record_prevents_another_generated_key(self):
        with tempfile.TemporaryDirectory() as temporary:
            home = Path(temporary)
            directory = home / ".config/desk"
            directory.mkdir(parents=True, mode=0o700)
            path = directory / "keychain-owner.jsonl"
            original = b"existing public setup record\n"
            path.write_bytes(original)
            store = Store()
            with patch.object(wallet.Path, "home", return_value=home), \
                    patch.object(wallet, "MacKeychain", return_value=store), \
                    patch.object(wallet, "generate") as generate, \
                    patch.object(sys, "argv", ["setup", "--create", "owner"]), \
                    contextlib.redirect_stderr(io.StringIO()):
                self.assertEqual(wallet.main(), 1)
            generate.assert_not_called()
            self.assertEqual(store.calls, [])
            self.assertEqual(path.read_bytes(), original)

    def test_native_call_has_exact_identity_and_create_only_refuses_duplicates(self):
        class FakeSecurity:
            def __init__(self, status):
                self.status = status
                self.attributes = None
                self.payload = None
                self.access = None
                self.later_calls = []

            def SecKeychainOpen(self, path, result):
                C.cast(result, C.POINTER(C.c_void_p)).contents.value = 17
                return 0

            def SecKeychainItemCreateFromContent(self, kind, attrs, size, raw, chain, access, item):
                self.assert_kind = kind
                listing = C.cast(attrs, C.POINTER(wallet.Attributes)).contents
                self.attributes = {
                    listing.attr[i].tag: C.string_at(listing.attr[i].data, listing.attr[i].length)
                    for i in range(listing.count)
                }
                self.payload = C.string_at(raw, size)
                self.access = access
                C.cast(item, C.POINTER(C.c_void_p)).contents.value = 23
                return self.status

            def SecKeychainItemCopyAccess(self, item, result):
                self.later_calls.append("copy access")
                C.cast(result, C.POINTER(C.c_void_p)).contents.value = 29
                return 0

        for status in (0, -25299):
            with self.subTest(status=status):
                store = wallet.MacKeychain.__new__(wallet.MacKeychain)
                store.sec = FakeSecurity(status)
                store.access = lambda label: 31
                store.release = lambda *args: None
                store.access_rules = lambda value: store.sec.later_calls.append("verify access")
                store.probe = lambda item: store.sec.later_calls.append("probe")
                if status:
                    with self.assertRaisesRegex(RuntimeError, "-25299"):
                        store.add("desk", PUBLIC, "Desk agent key", bytearray(FIXTURE.hex().encode()))
                    self.assertEqual(store.sec.later_calls, [])
                else:
                    store.add("desk", PUBLIC, "Desk agent key", bytearray(FIXTURE.hex().encode()))
                    self.assertEqual(store.sec.later_calls, ["copy access", "verify access", "probe"])
                self.assertEqual(store.sec.assert_kind, wallet.fourcc("genp"))
                self.assertEqual(store.sec.attributes, {
                    wallet.fourcc("svce"): b"desk", wallet.fourcc("acct"): PUBLIC.encode(),
                    wallet.fourcc("labl"): b"Desk agent key"})
                self.assertEqual(store.sec.payload, FIXTURE.hex().encode())
                self.assertEqual(store.sec.access, 31)

    @unittest.skipUnless(sys.platform == "darwin", "native Mac access objects")
    def test_native_access_requires_password_and_has_no_trusted_reader(self):
        store = wallet.MacKeychain()
        access = store.access("Desk public fixture test")
        try:
            self.assertGreaterEqual(store.access_rules(access), 1)
        finally:
            store.release(access)


class KeychainWalletReviewTests(unittest.TestCase):
    def test_invalid_keys_stop_before_cryptographic_backend(self):
        invalid = (FIXTURE[1:], b"\x00" + FIXTURE, bytes(32),
                   wallet.ORDER.to_bytes(32, "big"), (wallet.ORDER + 1).to_bytes(32, "big"))
        with patch.object(wallet.ec, "derive_private_key",
                          side_effect=AssertionError("invalid input reached crypto backend")) as derive:
            for value in invalid:
                with self.subTest(length=len(value)):
                    with self.assertRaisesRegex(wallet.SetupError, "invalid private scalar"):
                        wallet.address(value)
            derive.assert_not_called()

    def test_read_acl_without_password_requirement_is_rejected(self):
        class FakeCF:
            def CFEqual(self, left, right):
                return left == right

            def CFArrayGetCount(self, array):
                return 0

        class FakeSecurity:
            def __init__(self, flags):
                self.flags = flags

            def SecAccessCopyACLList(self, access, result):
                C.cast(result, C.POINTER(C.c_void_p)).contents.value = 1
                return 0

            def SecACLCopyAuthorizations(self, entry):
                return 2

            def SecACLCopyContents(self, entry, apps, description, flags):
                C.cast(apps, C.POINTER(C.c_void_p)).contents.value = 3
                C.cast(description, C.POINTER(C.c_void_p)).contents.value = 4
                C.cast(flags, C.POINTER(C.c_uint16)).contents.value = self.flags
                return 0

        for flags in (0, 1):
            with self.subTest(flags=flags):
                store = wallet.MacKeychain.__new__(wallet.MacKeychain)
                store.cf = FakeCF()
                store.sec = FakeSecurity(flags)
                store.read_tags = [99]
                store.values = lambda array: {1: [42], 2: [99]}[
                    array.value if isinstance(array, C.c_void_p) else array]
                store.release = lambda *references: None
                if flags == 0:
                    with self.assertRaisesRegex(RuntimeError, "password prompt is not required"):
                        store.access_rules(17)
                else:
                    self.assertEqual(store.access_rules(17), 1)

    def test_encoded_buffer_is_wiped_after_storage_success_or_failure(self):
        class RetainedBufferStore(Store):
            def add(self, service, account, label, secret):
                self.buffer = secret
                super().add(service, account, label, secret)

        for fail in (False, True):
            with self.subTest(fail=fail):
                store = RetainedBufferStore(fail=fail)
                if fail:
                    with self.assertRaisesRegex(RuntimeError, "key store refused"):
                        wallet.create("owner", store, lambda: bytearray(FIXTURE))
                else:
                    wallet.create("owner", store, lambda: bytearray(FIXTURE))
                self.assertEqual(store.calls[0][3], FIXTURE.hex().encode())
                self.assertEqual(store.buffer, bytes(64))

    def test_probe_wipes_returned_data_before_free_and_restores_interaction(self):
        class FakeSecurity:
            def __init__(self, status):
                self.status = status
                self.buffer = C.create_string_buffer(FIXTURE, len(FIXTURE))
                self.interactions = []
                self.freed = []

            def SecKeychainGetUserInteractionAllowed(self, result):
                C.cast(result, C.POINTER(C.c_ubyte)).contents.value = 1
                return 0

            def SecKeychainSetUserInteractionAllowed(self, allowed):
                self.interactions.append(allowed.value if isinstance(allowed, C.c_ubyte) else int(allowed))
                return 0

            def SecKeychainItemCopyContent(self, item, kind, attributes, size, data):
                C.cast(size, C.POINTER(C.c_uint32)).contents.value = len(FIXTURE)
                C.cast(data, C.POINTER(C.c_void_p)).contents.value = C.addressof(self.buffer)
                return self.status

            def SecKeychainItemFreeContent(self, attributes, data):
                self.freed.append(C.string_at(data, len(FIXTURE)))
                return 0

        for status in (-25308, 0, -25300):
            with self.subTest(status=status):
                store = wallet.MacKeychain.__new__(wallet.MacKeychain)
                store.sec = FakeSecurity(status)
                if status == -25308:
                    store.probe(17)
                else:
                    with self.assertRaisesRegex(RuntimeError, "did not refuse access"):
                        store.probe(17)
                self.assertEqual(store.sec.freed, [bytes(32)])
                self.assertEqual(store.sec.buffer.raw, bytes(32))
                self.assertEqual(store.sec.interactions, [0, 1])


if __name__ == "__main__":
    unittest.main()
