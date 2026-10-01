import importlib.util
import contextlib
import ctypes as C
import io
import json
import os
from pathlib import Path
import shutil
import stat
import subprocess
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
HEX = FIXTURE.hex().encode("ascii")
LETTERS = bytes.fromhex("0123456789abcdef" * 4)
FIXED_MESSAGE = ("Setup did not finish. Preserve any setup record "
                 "and inspect the named item before retrying.")


class Store:
    def __init__(self, fail=False):
        self.calls = []
        self.fail = fail

    def add(self, service, account, label, secret):
        self.calls.append((service, account, label, bytes(secret)))
        if self.fail:
            raise RuntimeError("key store refused")


class Input:
    """Stands in for sys.stdin. Keeps the byte stream where the helper reads it."""

    def __init__(self, data):
        self.buffer = io.BytesIO(data)


class Trickle(io.BytesIO):
    """A pipe that hands over one byte per read."""

    def readinto(self, buffer):
        return super().readinto(memoryview(buffer)[:1])


class Result:
    def __init__(self, code, out, err, keychain, stdin, files, buffers):
        self.code, self.out, self.err = code, out, err
        self.keychain, self.stdin, self.files, self.buffers = keychain, stdin, files, buffers


def run_main(*argv, data=b"", store=None, stdin=None, failure=None):
    """Runs main with a fake Keychain, fake standard input and an empty home directory.

    Every bytearray the helper makes is recorded so a test can check it was wiped.
    """
    made = []

    class Spy(bytearray):
        def __init__(self, *args):
            super().__init__(*args)
            made.append(self)

    out, err = io.StringIO(), io.StringIO()
    stdin = stdin or Input(data)
    store = Store() if store is None else store
    with tempfile.TemporaryDirectory() as temporary:
        (Path(temporary) / ".config/desk").mkdir(parents=True, mode=0o700)
        with patch.object(wallet.Path, "home", return_value=Path(temporary)), \
                patch.object(wallet, "MacKeychain", return_value=store,
                             side_effect=failure) as keychain, \
                patch.object(wallet, "bytearray", Spy, create=True), \
                patch.object(sys, "argv", ["helper", *argv]), \
                patch.object(sys, "stdin", stdin), \
                contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            try:
                code = wallet.main()
            except SystemExit as stop:
                code = stop.code
        files = sorted(str(path.relative_to(temporary)) for path in Path(temporary).rglob("*"))
    return Result(code, out.getvalue(), err.getvalue(), keychain, stdin, files, made)


class KeychainWalletTests(unittest.TestCase):
    def test_unexpected_errors_keep_fixed_message(self):
        for failure in (ValueError(FIXTURE.hex()), RuntimeError(FIXTURE.hex())):
            with self.subTest(error=type(failure).__name__):
                error = io.StringIO()
                with patch.object(wallet, "generate") as generate, \
                        patch.object(wallet, "MacKeychain", side_effect=failure), \
                        patch.object(sys, "argv", ["setup", "--store", "agent"]), \
                        patch.object(sys, "stdin", Input(HEX + b"\n")), \
                        contextlib.redirect_stderr(error):
                    self.assertEqual(wallet.main(), 1)
                self.assertEqual(json.loads(error.getvalue())["message"], FIXED_MESSAGE)
                self.assertNotIn("reason", json.loads(error.getvalue()))
                self.assertNotIn(FIXTURE.hex(), error.getvalue())
                generate.assert_not_called()

    def test_setup_failure_after_add_keeps_recovery_instruction_and_reason(self):
        reason = "new item did not refuse access without a prompt"

        class RefusedStore(Store):
            def add(self, *args):
                super().add(*args)
                raise wallet.SetupError(reason)

        store = RefusedStore()
        result = run_main("--store", "agent", data=HEX + b"\n", store=store)
        self.assertEqual(result.code, 1)
        self.assertEqual(len(store.calls), 1)
        stopped = json.loads(result.err)
        self.assertIn("inspect the named item", stopped["message"])
        self.assertEqual(stopped["reason"], reason)
        self.assertEqual(stopped["item"]["account"], PUBLIC)
        self.assertEqual(result.out, "")

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

        for status in (0, -25299, -25300):
            with self.subTest(status=status):
                store = wallet.MacKeychain.__new__(wallet.MacKeychain)
                store.sec = FakeSecurity(status)
                store.access = lambda label: 31
                store.release = lambda *args: None
                store.access_rules = lambda value: store.sec.later_calls.append("verify access")
                store.probe = lambda item: store.sec.later_calls.append("probe")
                if status:
                    with self.assertRaisesRegex(RuntimeError, str(status)) as raised:
                        store.add("desk", PUBLIC, "Desk agent key", bytearray(FIXTURE.hex().encode()))
                    # Only the duplicate status is told apart. Every other status is a plain failure.
                    self.assertIsInstance(raised.exception, wallet.SetupError)
                    self.assertEqual(isinstance(raised.exception, wallet.DuplicateItem),
                                     status == -25299)
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


class StoreModeTests(unittest.TestCase):
    def stored(self, account=PUBLIC, service="desk"):
        return ('{"account":"' + account + '","role":"agent","service":"' + service
                + '","state":"stored","prompt_required":true}\n')

    def test_stores_the_decoded_key_and_reports_public_fields_only(self):
        for data in (HEX, HEX + b"\n"):
            with self.subTest(newline=data.endswith(b"\n")):
                store = Store()
                result = run_main("--store", "agent", data=data, store=store)
                self.assertEqual(result.code, 0)
                self.assertEqual(result.out, self.stored())
                self.assertEqual(result.err, "")
                self.assertEqual(store.calls, [("desk", PUBLIC, "Desk agent key", HEX)])

    def test_reported_account_is_the_independently_derived_address(self):
        for key in (FIXTURE, LETTERS, bytes([0xFE]) * 32):
            with self.subTest(key_head=key[:1].hex()):
                expected = wallet.Account.from_key(key).address.lower()
                store = Store()
                result = run_main("--store", "agent", data=key.hex().encode() + b"\n", store=store)
                self.assertEqual(result.code, 0)
                self.assertEqual(result.out, self.stored(expected))
                self.assertEqual(store.calls, [("desk", expected, "Desk agent key",
                                                key.hex().encode())])
                self.assertNotIn(key.hex(), result.out + result.err)

    def test_short_reads_from_a_pipe_still_give_the_whole_key(self):
        stdin = Input(b"")
        stdin.buffer = Trickle(HEX + b"\n")
        store = Store()
        result = run_main("--store", "agent", stdin=stdin, store=store)
        self.assertEqual(result.code, 0)
        self.assertEqual(store.calls, [("desk", PUBLIC, "Desk agent key", HEX)])

    def test_refuses_bad_input_before_any_keychain_call_and_reads_at_most_66_bytes(self):
        upper_one = LETTERS.hex().encode().replace(b"a", b"A", 1)
        bad = {
            "empty": b"",
            "newline only": b"\n",
            "63 characters": HEX[:63],
            "63 characters and newline": HEX[:63] + b"\n",
            "65 characters": HEX + b"0",
            "65 characters and newline": HEX + b"0\n",
            "upper case": LETTERS.hex().upper().encode(),
            "one upper case letter": upper_one,
            "0x prefix": b"0x" + HEX[:62],
            "0x prefix and 64 characters": b"0x" + HEX,
            "two newlines": HEX + b"\n\n",
            "carriage return": HEX + b"\r\n",
            "trailing space": HEX + b" ",
            "newline inside": HEX[:30] + b"\n" + HEX[31:],
            "leading space": b" " + HEX[:63],
            "non hex letter": HEX[:63] + b"g",
            "null byte": HEX[:63] + b"\x00",
            "non ascii": HEX[:62] + "é".encode(),
            "newline then more input": HEX + b"\n" + b"x",
            "key then more input": HEX + b"x" * 1000,
            "long input": b"a" * 5000,
        }
        for name, data in bad.items():
            with self.subTest(name):
                result = run_main("--store", "agent", data=data)
                self.assertEqual(result.code, 1)
                self.assertFalse(result.keychain.called)
                self.assertEqual(result.out, "")
                self.assertEqual(json.loads(result.err)["message"], FIXED_MESSAGE)
                self.assertNotIn(HEX.decode(), result.err)
                self.assertLessEqual(result.stdin.buffer.tell(), 66)
                self.assertEqual(result.files, [".config", ".config/desk"])
                self.assertIn(66, [len(buffer) for buffer in result.buffers])
                self.assertTrue(all(not any(buffer) for buffer in result.buffers))

    def test_every_buffer_is_wiped_on_every_path(self):
        class Existing(Store):
            def add(self, *args):
                super().add(*args)
                raise wallet.DuplicateItem("key store status -25299")

        # The sizes are the input buffer, the decoded key and the hex text handed to the store.
        cases = {
            "stored": (HEX + b"\n", Store(), 0, {66, 32, 64}),
            "store refused": (HEX, Store(fail=True), 1, {66, 32, 64}),
            "already stored": (HEX, Existing(), 3, {66, 32, 64}),
            "keychain unavailable": (HEX, None, 1, {66, 32}),
            "zero key": (b"0" * 64, Store(), 1, {66, 32}),
            "key above the curve order": (b"f" * 64, Store(), 1, {66, 32}),
            "bad input": (HEX[:-1], Store(), 1, {66}),
        }
        for name, (data, store, code, sizes) in cases.items():
            with self.subTest(name):
                failure = None if store else wallet.SetupError("Mac key store required")
                result = run_main("--store", "agent", data=data, store=store, failure=failure)
                self.assertEqual(result.code, code)
                self.assertTrue(sizes <= {len(buffer) for buffer in result.buffers})
                self.assertTrue(all(not any(buffer) for buffer in result.buffers))
                self.assertNotIn(data.strip().decode(), result.out + result.err)

    def test_duplicate_item_gives_the_exists_reply_and_exit_code_3(self):
        class Existing(Store):
            def add(self, *args):
                super().add(*args)
                raise wallet.DuplicateItem("key store status -25299")

        store = Existing()
        result = run_main("--store", "agent", data=HEX + b"\n", store=store)
        self.assertEqual(result.code, 3)
        self.assertEqual(result.out, '{"account":"' + PUBLIC + '","state":"exists"}\n')
        self.assertEqual(result.err, "")
        self.assertEqual(len(store.calls), 1)

    def test_any_other_failure_is_exit_1_with_nothing_on_stdout(self):
        for failure in (wallet.SetupError("key store status -25300"),
                        wallet.SetupError("key store status -25293"),
                        RuntimeError("key store refused")):
            with self.subTest(failure=str(failure)):
                class Failing(Store):
                    def add(self, *args):
                        super().add(*args)
                        raise failure

                result = run_main("--store", "agent", data=HEX + b"\n", store=Failing())
                self.assertEqual(result.code, 1)
                self.assertEqual(result.out, "")
                self.assertEqual(json.loads(result.err)["message"], FIXED_MESSAGE)

    def test_store_mode_writes_no_setup_record_file(self):
        for store in (Store(), Store(fail=True)):
            result = run_main("--store", "agent", data=HEX + b"\n", store=store)
            self.assertEqual(result.files, [".config", ".config/desk"])

    def test_store_accepts_only_the_agent_role(self):
        for role in ("owner", "admin", ""):
            with self.subTest(role=role):
                result = run_main("--store", role, data=HEX + b"\n")
                self.assertEqual(result.code, 2)
                self.assertFalse(result.keychain.called)
                self.assertEqual(result.stdin.buffer.tell(), 0)
                self.assertEqual(result.out, "")
        self.assertEqual(run_main("--store", "agent", data=HEX + b"\n").code, 0)

    def test_service_defaults_to_desk_and_takes_the_allowed_alphabet(self):
        for name in ("desk", "desk-test", "A.b_c-9", "x", "a" * 80):
            with self.subTest(service=name):
                store = Store()
                result = run_main("--store", "agent", "--service", name,
                                  data=HEX + b"\n", store=store)
                self.assertEqual(result.code, 0)
                self.assertEqual(result.out, self.stored(service=name))
                self.assertEqual(store.calls, [(name, PUBLIC, "Desk agent key", HEX)])

    def test_bad_service_names_stop_before_input_and_keychain(self):
        for name in ("", "a" * 81, "a b", "a/b", "a\n", "é", "a;b", "a\x00b", "$HOME"):
            with self.subTest(service=name):
                result = run_main("--store", "agent", "--service", name, data=HEX + b"\n")
                self.assertEqual(result.code, 2)
                self.assertFalse(result.keychain.called)
                self.assertEqual(result.stdin.buffer.tell(), 0)
                self.assertEqual(result.out, "")

    def test_create_no_longer_parses(self):
        for role in ("agent", "owner"):
            with self.subTest(role=role):
                result = run_main("--create", role, data=HEX)
                self.assertEqual(result.code, 2)
                self.assertFalse(result.keychain.called)
                self.assertEqual(result.stdin.buffer.tell(), 0)
                self.assertEqual(result.out, "")
                self.assertEqual(result.files, [".config", ".config/desk"])

    def test_check_access_still_works_and_never_reads_input(self):
        class Native:
            released = ()

            def access(self, label):
                self.label = label
                return 5

            def access_rules(self, access, configure=False):
                return 2

            def release(self, *references):
                self.released = references

        native = Native()
        result = run_main("--check-access", data=HEX, store=native)
        self.assertEqual(result.code, 0)
        self.assertEqual(json.loads(result.out), {
            "read_rules": 2, "password_required": True, "trusted_readers": 0,
            "keychain_items_created": 0})
        self.assertEqual(native.label, "Desk access preparation")
        self.assertEqual(native.released, (5,))
        self.assertEqual(result.stdin.buffer.tell(), 0)

    def test_check_access_and_store_exclude_each_other(self):
        result = run_main("--check-access", "--store", "agent", data=HEX)
        self.assertEqual(result.code, 2)
        self.assertFalse(result.keychain.called)


ROOT = Path(__file__).parents[1]
MAKE = shutil.which("make")
REAL_HOME = os.path.expanduser("~")


@unittest.skipUnless(MAKE, "make is required")
class InstallLauncherTests(unittest.TestCase):
    """Runs make install-launcher against a temporary home. It never builds.

    HOME is set both in the environment and on the make command line, and a dry run
    comes first so a recipe that names the build or the real home stops the test.
    """

    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="desk-install-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.home = self.root / "home"
        self.home.mkdir()
        self.bin = self.root / "bin"
        self.args = self.root / "python-args"

    def fake_python(self, directory, status=0):
        directory.mkdir(parents=True, exist_ok=True)
        python = directory / "python3"
        python.write_text("#!/bin/sh\nprintf '%s\\n' \"$@\" >> '" + str(self.args)
                          + "'\nexit " + str(status) + "\n")
        python.chmod(0o755)
        return python

    def make(self, *arguments, path=None, home=None):
        home = home or self.home
        env = {"HOME": str(home), "PATH": path or str(self.bin) + ":/usr/bin:/bin"}
        return subprocess.run([MAKE, *arguments, "HOME=" + str(home)], cwd=ROOT, env=env,
                              capture_output=True, text=True, timeout=60)

    def install(self, **options):
        home = options.get("home") or self.home
        dry = self.make("-n", "install-launcher", **options)
        self.assertEqual(dry.returncode, 0, dry.stderr)
        shown = dry.stdout.replace(str(home), "")
        for forbidden in (REAL_HOME, "scala-cli", "bin/desk.jar"):
            self.assertNotIn(forbidden, shown)
        return self.make("install-launcher", **options)

    def launch(self, *arguments, **variables):
        java = self.root / "java"
        java.write_text("#!/bin/sh\nprintf '%s\\n' \"$DESK_PYTHON\" \"$DESK_KEY_HELPER\" \"$@\"\n")
        java.chmod(0o755)
        env = {"HOME": str(self.home), "PATH": "/usr/bin:/bin", "DESK_JAVA": str(java), **variables}
        done = subprocess.run([str(self.home / ".local/bin/desk"), *arguments], env=env,
                              capture_output=True, text=True, timeout=60)
        self.assertEqual(done.returncode, 0, done.stderr)
        return done.stdout.splitlines()

    def test_helper_is_copied_with_owner_only_mode(self):
        self.fake_python(self.bin)
        helper = self.home / ".local/lib/desk-keychain.py"
        for old in (None, b"old helper"):
            with self.subTest(replacing=old):
                helper.parent.mkdir(parents=True, exist_ok=True)
                if old:
                    helper.write_bytes(old)
                    helper.chmod(0o644)
                done = self.install()
                self.assertEqual(done.returncode, 0, done.stderr)
                self.assertFalse(helper.is_symlink())
                self.assertEqual(helper.read_bytes(), (ROOT / "tools/keychain_wallet.py").read_bytes())
                self.assertEqual(stat.S_IMODE(helper.stat().st_mode), 0o600)

    def test_install_builds_first_then_runs_the_launcher_part_then_copies_the_jar(self):
        # A dry run only prints the recipe, so nothing is built or installed.
        dry = self.make("-n", "install")
        self.assertEqual(dry.returncode, 0, dry.stderr)
        shown = dry.stdout
        build = shown.index("scala-cli")
        launcher = shown.index("keychain_wallet.py")
        jar = shown.index("cp bin/desk.jar")
        self.assertTrue(build < launcher < jar)

    def test_the_found_python_must_import_the_three_libraries(self):
        self.fake_python(self.bin)
        self.assertEqual(self.install().returncode, 0)
        self.assertEqual(self.args.read_text().splitlines(),
                         ["-c", "import cryptography, Crypto, eth_account"])

    def test_a_failed_import_stops_the_install_and_keeps_the_old_files(self):
        self.fake_python(self.bin, status=1)
        done = self.install()
        self.assertNotEqual(done.returncode, 0)
        self.assertFalse((self.home / ".local").exists())
        launcher = self.home / ".local/bin/desk"
        launcher.parent.mkdir(parents=True)
        launcher.write_bytes(b"old launcher")
        self.assertNotEqual(self.install().returncode, 0)
        self.assertEqual(launcher.read_bytes(), b"old launcher")
        self.assertFalse((self.home / ".local/lib").exists())

    def test_a_missing_python_stops_the_install(self):
        tools = self.root / "tools"
        tools.mkdir()
        for name in ("mkdir", "cp", "chmod"):
            (tools / name).symlink_to(shutil.which(name))
        done = self.install(path=str(tools))
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("python3 not found", done.stderr)
        self.assertFalse((self.home / ".local").exists())

    def test_a_path_that_could_change_the_launcher_stops_the_install(self):
        odd_python = self.fake_python(self.root / "a$b").parent
        odd_home = self.root / "home`x"
        odd_home.mkdir()
        self.fake_python(self.bin)
        for name, options, home in (
                ("python path", {"path": str(odd_python) + ":/usr/bin:/bin"}, self.home),
                ("home path", {"home": odd_home}, odd_home)):
            with self.subTest(name):
                done = self.install(**options)
                self.assertNotEqual(done.returncode, 0)
                self.assertIn("unsafe character", done.stderr)
                self.assertFalse((home / ".local").exists())

    def test_launcher_text_keeps_the_jar_line_and_names_absolute_defaults(self):
        python = self.fake_python(self.bin)
        self.assertEqual(self.install().returncode, 0)
        launcher = self.home / ".local/bin/desk"
        helper = self.home / ".local/lib/desk-keychain.py"
        self.assertTrue(os.access(launcher, os.X_OK))
        lines = launcher.read_text().splitlines()
        self.assertEqual(lines, [
            "#!/bin/sh",
            'export DESK_PYTHON="${DESK_PYTHON:-' + str(python) + '}"',
            'export DESK_KEY_HELPER="${DESK_KEY_HELPER:-' + str(helper) + '}"',
            'exec "${DESK_JAVA:-/opt/homebrew/opt/openjdk/bin/java}" -jar '
            '"$HOME/.local/lib/desk.jar" "$@"'])

    def test_launcher_exports_the_defaults_and_passes_arguments_on(self):
        python = self.fake_python(self.bin)
        self.assertEqual(self.install().returncode, 0)
        helper = self.home / ".local/lib/desk-keychain.py"
        lines = self.launch("--keys")
        self.assertEqual(lines, [
            str(python), str(helper), "-jar", str(self.home / ".local/lib/desk.jar"), "--keys"])
        self.assertTrue(all(os.path.isabs(line) for line in lines[:2]))

    def test_launcher_keeps_a_value_that_is_already_set(self):
        python = self.fake_python(self.bin)
        self.assertEqual(self.install().returncode, 0)
        helper = self.home / ".local/lib/desk-keychain.py"
        lines = self.launch(DESK_PYTHON="/custom/python3", DESK_KEY_HELPER="/custom/helper.py")
        self.assertEqual(lines[:2], ["/custom/python3", "/custom/helper.py"])
        # One value set and the other not. An empty value counts as not set.
        lines = self.launch(DESK_PYTHON="/custom/python3", DESK_KEY_HELPER="")
        self.assertEqual(lines[:2], ["/custom/python3", str(helper)])
        self.assertNotEqual(lines[0], str(python))


if __name__ == "__main__":
    unittest.main()
