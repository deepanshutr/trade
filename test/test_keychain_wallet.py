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
FIXED_MESSAGE = "Storage did not finish. Inspect the named Keychain item before retrying."


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


READ_TAG = 99


class Native:
    """Stands in for CoreFoundation and Security around one access object.

    The object holds `rules` rules. Each rule has the authorizations in `tags`, where
    READ_TAG is the read tag. The trusted application list is null when `apps` is 0,
    else it holds `listed` programs. `flags` is the rule's prompt flags. `read_status`
    is what a read with no prompt returns. When `honor_set` is False the system drops
    the flags it is asked to set.
    """

    def __init__(self, flags=0, listed=0, apps=3, tags=(READ_TAG,), rules=1,
                 read_status=-25293, honor_set=True):
        self.flags, self.listed, self.apps, self.tags = flags, listed, apps, tags
        self.rules, self.read_status, self.honor_set = rules, read_status, honor_set
        self.set_flags = []
        self.interactions = []
        self.buffer = C.create_string_buffer(FIXTURE, len(FIXTURE))

    def values(self, array):
        handle = array.value if isinstance(array, C.c_void_p) else array
        return {1: [42] * self.rules, 2: list(self.tags)}[handle]

    def CFEqual(self, left, right):
        return left == right

    def CFArrayGetCount(self, array):
        return self.listed

    def CFStringCreateWithCString(self, allocator, text, encoding):
        return 5

    def CFArrayCreate(self, allocator, values, count, callbacks):
        return 6

    def SecAccessCreate(self, title, trusted, result):
        C.cast(result, C.POINTER(C.c_void_p)).contents.value = 7
        return 0

    def SecAccessCopyACLList(self, access, result):
        C.cast(result, C.POINTER(C.c_void_p)).contents.value = 1
        return 0

    def SecACLCopyAuthorizations(self, entry):
        return 2

    def SecACLCopyContents(self, entry, apps, description, flags):
        C.cast(apps, C.POINTER(C.c_void_p)).contents.value = self.apps
        C.cast(description, C.POINTER(C.c_void_p)).contents.value = 4
        C.cast(flags, C.POINTER(C.c_uint16)).contents.value = self.flags
        return 0

    def SecACLSetContents(self, entry, apps, description, flags):
        self.set_flags.append(flags)
        if self.honor_set:
            self.flags = flags
        return 0

    def SecKeychainOpen(self, path, result):
        C.cast(result, C.POINTER(C.c_void_p)).contents.value = 17
        return 0

    def SecKeychainItemCreateFromContent(self, kind, attrs, size, raw, chain, access, item):
        C.cast(item, C.POINTER(C.c_void_p)).contents.value = 23
        return 0

    def SecKeychainItemCopyAccess(self, item, result):
        C.cast(result, C.POINTER(C.c_void_p)).contents.value = 29
        return 0

    def SecKeychainGetUserInteractionAllowed(self, result):
        C.cast(result, C.POINTER(C.c_ubyte)).contents.value = 1
        return 0

    def SecKeychainSetUserInteractionAllowed(self, allowed):
        self.interactions.append(allowed.value if isinstance(allowed, C.c_ubyte) else int(allowed))
        return 0

    def SecKeychainItemCopyContent(self, item, kind, attributes, size, data):
        # A refused read hands back no bytes. Only a read that succeeds does.
        if self.read_status == 0:
            C.cast(size, C.POINTER(C.c_uint32)).contents.value = len(FIXTURE)
            C.cast(data, C.POINTER(C.c_void_p)).contents.value = C.addressof(self.buffer)
        return self.read_status

    def SecKeychainItemFreeContent(self, attributes, data):
        return 0


def native_store(**options):
    native = Native(**options)
    store = wallet.MacKeychain.__new__(wallet.MacKeychain)
    store.cf = store.sec = native
    store.read_tags = [READ_TAG]
    store.values = native.values
    store.release = lambda *references: None
    return store, native


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
        self.assertEqual(stopped["message"], FIXED_MESSAGE)
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
        # -25308 is errSecInteractionNotAllowed. -25293 is errSecAuthFailed, which macOS 27
        # returns for a stored item whose read rule has an empty trusted list.
        for status in (-25308, -25293):
            with self.subTest(refused=status):
                self.assertTrue(wallet.acceptable_read_status(status))
        # 0 is a read that worked. -25300 is an item that is not there.
        for status in (0, -25300, -25299, -50, 1):
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
                # The saved item is checked once, on the saved access only, with the
                # relaxed rule. The in-memory access object (31) is never checked here.
                checks = []

                def verify(value, **options):
                    checks.append((value.value, options))
                    store.sec.later_calls.append("verify access")

                store.access_rules = verify
                store.probe = lambda item: store.sec.later_calls.append("probe")
                if status:
                    with self.assertRaisesRegex(RuntimeError, str(status)) as raised:
                        store.add("desk", PUBLIC, "Desk agent key", bytearray(FIXTURE.hex().encode()))
                    # Only the duplicate status is told apart. Every other status is a plain failure.
                    self.assertIsInstance(raised.exception, wallet.SetupError)
                    self.assertEqual(isinstance(raised.exception, wallet.DuplicateItem),
                                     status == -25299)
                    self.assertEqual(store.sec.later_calls, [])
                    self.assertEqual(checks, [])
                else:
                    store.add("desk", PUBLIC, "Desk agent key", bytearray(FIXTURE.hex().encode()))
                    self.assertEqual(store.sec.later_calls, ["copy access", "verify access", "probe"])
                    self.assertEqual(checks, [(29, {"require_password": False})])
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

    def test_in_memory_check_rejects_a_read_rule_without_the_password_flag(self):
        # This was the stored-item check test. The stored item no longer needs the flag, so
        # it now targets the strict check that MacKeychain.access runs on its own object.
        for flags, accepted in ((0, False), (0x100, False), (1, True), (0x101, True)):
            for options in ({}, {"require_password": True}):
                with self.subTest(flags=flags, options=options):
                    store, _ = native_store(flags=flags)
                    if accepted:
                        self.assertEqual(store.access_rules(17, **options), 1)
                    else:
                        with self.assertRaisesRegex(RuntimeError, "password prompt is not required"):
                            store.access_rules(17, **options)

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

        # -25308 and -25293 are refusals. 0 is a read that worked and -25300 is no item.
        for status in (-25308, -25293, 0, -25300):
            with self.subTest(status=status):
                store = wallet.MacKeychain.__new__(wallet.MacKeychain)
                store.sec = FakeSecurity(status)
                if status in (-25308, -25293):
                    store.probe(17)
                else:
                    with self.assertRaisesRegex(RuntimeError, "did not refuse access"):
                        store.probe(17)
                self.assertEqual(store.sec.freed, [bytes(32)])
                self.assertEqual(store.sec.buffer.raw, bytes(32))
                self.assertEqual(store.sec.interactions, [0, 1])


class StoredItemRuleTests(unittest.TestCase):
    """The stored item has no trusted program and refuses a read without a prompt.

    macOS 27 stores the read rule with prompt flags 0x100 and drops the 0x1 flag. The
    stored-item check therefore skips the flag and keeps every other condition.
    """

    def store_with(self, **options):
        store, _ = native_store(**options)
        return store

    def test_stored_check_accepts_flags_0x100_with_an_empty_application_list(self):
        for flags in (0x100, 0, 1, 0x101):
            with self.subTest(flags=flags):
                store, native = native_store(flags=flags)
                self.assertEqual(store.access_rules(17, require_password=False), 1)
                self.assertEqual(native.set_flags, [])

    def test_stored_check_counts_every_read_rule(self):
        store, _ = native_store(flags=0x100, rules=2)
        self.assertEqual(store.access_rules(17, require_password=False), 2)

    def test_stored_check_still_refuses_a_non_empty_application_list(self):
        for flags in (0x100, 1):
            with self.subTest(flags=flags):
                store = self.store_with(flags=flags, listed=1)
                with self.assertRaisesRegex(RuntimeError, "a key reader is preauthorized"):
                    store.access_rules(17, require_password=False)

    def test_stored_check_still_refuses_a_null_application_list(self):
        # A null list means any application may read.
        for flags in (0x100, 1):
            with self.subTest(flags=flags):
                store = self.store_with(flags=flags, apps=0)
                with self.assertRaisesRegex(RuntimeError, "a key reader is preauthorized"):
                    store.access_rules(17, require_password=False)

    def test_stored_check_still_needs_a_read_rule(self):
        for name, options in (("no rule at all", {"rules": 0}),
                              ("a rule that cannot read", {"tags": (98,)})):
            with self.subTest(name):
                store = self.store_with(flags=0x100, **options)
                with self.assertRaisesRegex(RuntimeError, "no secret read authorization found"):
                    store.access_rules(17, require_password=False)

    def test_in_memory_check_keeps_the_application_list_and_read_rule_conditions(self):
        cases = (("non-empty list", {"listed": 1}, "a key reader is preauthorized"),
                 ("null list", {"apps": 0}, "a key reader is preauthorized"),
                 ("no rule at all", {"rules": 0}, "no secret read authorization found"),
                 ("a rule that cannot read", {"tags": (98,)}, "no secret read authorization found"))
        for name, options, message in cases:
            for call in ({}, {"require_password": True}):
                with self.subTest(name, call=call):
                    store = self.store_with(flags=1, **options)
                    with self.assertRaisesRegex(RuntimeError, message):
                        store.access_rules(17, **call)

    def test_require_password_is_keyword_only_and_defaults_to_strict(self):
        store, _ = native_store(flags=0x100)
        with self.assertRaises(TypeError):
            store.access_rules(17, False, False)
        with self.assertRaisesRegex(RuntimeError, "password prompt is not required"):
            store.access_rules(17)

    def test_configure_still_sets_the_password_flag(self):
        for flags in (0, 0x100):
            for options in ({}, {"require_password": True}, {"require_password": False}):
                with self.subTest(flags=flags, options=options):
                    store, native = native_store(flags=flags)
                    self.assertEqual(store.access_rules(17, configure=True, **options), 1)
                    self.assertEqual(native.set_flags, [flags | 1])

    def test_check_mode_never_sets_a_flag(self):
        for options in ({}, {"require_password": False}):
            with self.subTest(options=options):
                store, native = native_store(flags=1)
                store.access_rules(17, **options)
                self.assertEqual(native.set_flags, [])

    def test_the_in_memory_object_is_built_with_the_strict_check(self):
        # If the system kept the flag it was asked to set, the object passes.
        store, native = native_store(flags=0)
        self.assertEqual(store.access("Desk test").value, 7)
        self.assertEqual(native.set_flags, [1])
        # If the system dropped the flag, the strict check refuses the object. The relaxed
        # check would have let it through.
        store, native = native_store(flags=0, honor_set=False)
        with self.assertRaisesRegex(RuntimeError, "password prompt is not required"):
            store.access("Desk test")

    def test_add_accepts_the_item_macos_27_stores(self):
        # Flags 0x100, an empty application list, and a read with no prompt that returns
        # -25293 with no bytes. This is the stored item that failed before.
        for status in (-25293, -25308):
            with self.subTest(status=status):
                store, native = native_store(flags=0x100, read_status=status)
                store.access = lambda label: 31
                store.add("desk", PUBLIC, "Desk agent key", bytearray(HEX))
                self.assertEqual(native.interactions, [0, 1])

    def test_add_refuses_a_stored_item_that_is_not_locked_down(self):
        cases = (("trusted program", {"listed": 1}, "a key reader is preauthorized"),
                 ("any application", {"apps": 0}, "a key reader is preauthorized"),
                 ("no read rule", {"rules": 0}, "no secret read authorization found"),
                 ("read works", {"read_status": 0}, "did not refuse access"),
                 ("no item", {"read_status": -25300}, "did not refuse access"))
        for name, options, message in cases:
            with self.subTest(name):
                store, _ = native_store(flags=0x100, **options)
                store.access = lambda label: 31
                with self.assertRaisesRegex(RuntimeError, message):
                    store.add("desk", PUBLIC, "Desk agent key", bytearray(HEX))


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
# Launchers earlier installs wrote as desk: the first form without the attach flag, and the
# form since key setup. Windows opens Display settings for a bare desk, so the command is trade
# on every system, and an install removes a desk that names the Desk jar.
FIRST_LAUNCHER = ('#!/bin/sh\nexec "${DESK_JAVA:-/opt/homebrew/opt/openjdk/bin/java}" '
                  '-jar "$HOME/.local/lib/desk.jar" "$@"\n')
EARLIER_LAUNCHER = (
    '#!/bin/sh\nexport DESK_PYTHON="${DESK_PYTHON:-/usr/bin/python3}"\n'
    'exec "${DESK_JAVA:-/opt/homebrew/opt/openjdk/bin/java}" -XX:+DisableAttachMechanism '
    '-jar "$HOME/.local/lib/desk.jar" "$@"\n')
# Texts that do not name the Desk jar. The last one matches only if dots read as any character.
NOT_THE_JAR = (("another program", "#!/bin/sh\necho another program\n"),
               ("names desk only", '#!/bin/sh\nexec /opt/desk/bin/desk "$@"\n'),
               ("dots read as any character", "#!/bin/sh\n# /opt/xlocal/lib/desk-jar\n"))


@unittest.skipUnless(MAKE, "make is required")
class InstallLauncherTests(unittest.TestCase):
    """Runs make install-launcher against a temporary home. It never builds.

    HOME is set both in the environment and on the make command line, and a dry run
    comes first so a recipe that names the build or the real home stops the test.
    A stand-in uname that prints Darwin comes first on every PATH, so the class runs alike
    on the Mac and on Linux, where make needs a named Java.
    """

    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="desk-install-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.home = self.root / "home"
        self.home.mkdir()
        self.bin = self.root / "bin"
        self.args = self.root / "python-args"
        self.uname = self.root / "uname"
        self.uname.mkdir()
        (self.uname / "uname").write_text(
            "#!/bin/sh\n[ \"$#\" = 1 ] && [ \"$1\" = -s ] || exit 1\nprintf '%s\\n' Darwin\n")
        (self.uname / "uname").chmod(0o755)

    def fake_python(self, directory, status=0, executable=None):
        """A python3 that records its arguments. Asked for sys.executable it prints
        the given path, or its own path, which is what a real python3 reports. The library
        import alone exits with status, so only the import check can stop the install."""
        directory.mkdir(parents=True, exist_ok=True)
        python = directory / "python3"
        reported = str(python) if executable is None else str(executable)
        python.write_text("#!/bin/sh\nprintf '%s\\n' \"$@\" >> '" + str(self.args)
                          + "'\ncase \"$2\" in *sys.executable*) printf '%s\\n' '" + reported
                          + "';; esac\ncase \"$2\" in 'import cryptography'*) exit " + str(status)
                          + ";; esac\n")
        python.chmod(0o755)
        return python

    def make(self, *arguments, path=None, home=None):
        home = home or self.home
        env = {"HOME": str(home),
               "PATH": str(self.uname) + ":" + (path or str(self.bin) + ":/usr/bin:/bin")}
        return subprocess.run([MAKE, *arguments, "HOME=" + str(home)], cwd=ROOT, env=env,
                              capture_output=True, text=True, timeout=60)

    def install(self, *arguments, **options):
        home = options.get("home") or self.home
        dry = self.make("-n", "install-launcher", *arguments, **options)
        self.assertEqual(dry.returncode, 0, dry.stderr)
        shown = dry.stdout.replace(str(home), "")
        for forbidden in (REAL_HOME, "scala-cli", "bin/desk.jar"):
            self.assertNotIn(forbidden, shown)
        return self.make("install-launcher", *arguments, **options)

    def launch(self, *arguments, **variables):
        java = self.root / "java"
        java.write_text("#!/bin/sh\nprintf '%s\\n' \"$DESK_PYTHON\" \"$DESK_KEY_HELPER\" \"$@\"\n")
        java.chmod(0o755)
        env = {"HOME": str(self.home), "PATH": "/usr/bin:/bin", "DESK_JAVA": str(java), **variables}
        done = subprocess.run([str(self.home / ".local/bin/trade"), *arguments], env=env,
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
        self.assertEqual(self.args.read_text().splitlines()[:2],
                         ["-c", "import cryptography, Crypto, eth_account"])

    def test_a_failed_import_stops_the_install_and_keeps_the_old_files(self):
        self.fake_python(self.bin, status=1)
        done = self.install()
        self.assertNotEqual(done.returncode, 0)
        self.assertFalse((self.home / ".local").exists())
        launcher = self.home / ".local/bin/trade"
        earlier = self.home / ".local/bin/desk"
        launcher.parent.mkdir(parents=True)
        launcher.write_text(EARLIER_LAUNCHER)
        earlier.write_text(EARLIER_LAUNCHER)
        self.assertNotEqual(self.install().returncode, 0)
        self.assertEqual(launcher.read_text(), EARLIER_LAUNCHER)
        self.assertEqual(earlier.read_text(), EARLIER_LAUNCHER)
        self.assertFalse((self.home / ".local/lib").exists())

    def test_the_launcher_is_trade_and_a_desk_that_names_the_jar_is_removed(self):
        self.fake_python(self.bin)
        folder = self.home / ".local/bin"
        for name, earlier in (("no desk", None), ("first form", FIRST_LAUNCHER),
                              ("key setup form", EARLIER_LAUNCHER)):
            with self.subTest(name):
                folder.mkdir(parents=True, exist_ok=True)
                (folder / "trade").unlink(missing_ok=True)
                if earlier:
                    (folder / "desk").write_text(earlier)
                done = self.install()
                self.assertEqual(done.returncode, 0, done.stderr)
                self.assertEqual(done.stderr, "")
                self.assertTrue(os.access(folder / "trade", os.X_OK))
                self.assertFalse((folder / "desk").exists())
                # The removal says so in one line, and no line of the old file is printed.
                # make also prints the recipe, so only an exact line counts.
                said = done.stdout.splitlines()
                notice = "Removed the earlier launcher " + str(folder / "desk") + ". The command is trade."
                self.assertEqual(said.count(notice), 1 if earlier else 0)
                for line in (earlier or "").splitlines():
                    self.assertNotIn(line, said)

    def test_a_desk_that_does_not_name_the_jar_is_kept(self):
        self.fake_python(self.bin)
        folder = self.home / ".local/bin"
        other = folder / "desk"
        folder.mkdir(parents=True)
        for name, text in NOT_THE_JAR:
            with self.subTest(name):
                other.write_text(text)
                done = self.install()
                self.assertEqual(done.returncode, 0, done.stderr)
                self.assertEqual(other.read_text(), text)
        other.unlink()
        other.mkdir()
        done = self.install()
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertTrue(other.is_dir())

    def test_a_launcher_that_cannot_be_written_leaves_the_earlier_desk(self):
        # The launcher is written to trade.new and moved onto trade. A folder there makes the
        # write fail after every check, so the earlier desk must still be in place.
        self.fake_python(self.bin)
        folder = self.home / ".local/bin"
        (folder / "trade.new").mkdir(parents=True)
        (folder / "desk").write_text(EARLIER_LAUNCHER)
        self.assertNotEqual(self.install().returncode, 0)
        self.assertEqual((folder / "desk").read_text(), EARLIER_LAUNCHER)
        self.assertFalse((folder / "trade").exists())

    def test_a_trade_that_does_not_name_the_jar_stops_the_install(self):
        self.fake_python(self.bin)
        folder = self.home / ".local/bin"
        trade = folder / "trade"
        elsewhere = self.root / "elsewhere"
        cases = [(name, lambda text=text: trade.write_text(text)) for name, text in NOT_THE_JAR]
        cases += [("a folder", trade.mkdir),
                  ("a link to another program", lambda: trade.symlink_to(elsewhere)),
                  ("a dangling link", lambda: trade.symlink_to(self.root / "missing"))]
        # A file that cannot be read cannot be shown to be a Desk launcher.
        cases.append(("unreadable", lambda: (trade.write_text(EARLIER_LAUNCHER), trade.chmod(0))))
        root = hasattr(os, "geteuid") and os.geteuid() == 0
        for name, make in cases:
            with self.subTest(name):
                if name == "unreadable" and root:
                    self.skipTest("root reads any file")
                shutil.rmtree(self.home / ".local", ignore_errors=True)
                folder.mkdir(parents=True)
                elsewhere.write_text("#!/bin/sh\necho mine\n")
                make()
                (folder / "desk").write_text(EARLIER_LAUNCHER)
                done = self.install()
                self.assertNotEqual(done.returncode, 0)
                self.assertIn(str(trade) + " is not a Desk launcher", done.stderr)
                self.assertNotIn("grep", done.stderr)
                self.assertFalse((self.home / ".local/lib").exists())
                self.assertEqual((folder / "desk").read_text(), EARLIER_LAUNCHER)
                self.assertEqual(elsewhere.read_text(), "#!/bin/sh\necho mine\n")
                self.assertTrue(trade.is_symlink() or trade.exists())

    def test_a_reinstall_prints_no_line_of_the_launcher_it_replaces(self):
        self.fake_python(self.bin)
        self.assertEqual(self.install().returncode, 0)
        old = (self.home / ".local/bin/trade").read_text().splitlines()
        done = self.install()
        self.assertEqual(done.returncode, 0, done.stderr)
        for line in old:
            self.assertNotIn(line, done.stdout.splitlines())

    def test_a_link_named_trade_is_replaced_and_not_written_through(self):
        self.fake_python(self.bin)
        folder = self.home / ".local/bin"
        folder.mkdir(parents=True)
        target = self.root / "stopgap"
        target.write_text(EARLIER_LAUNCHER)
        (folder / "trade").symlink_to(target)
        done = self.install()
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertFalse((folder / "trade").is_symlink())
        self.assertTrue(os.access(folder / "trade", os.X_OK))
        self.assertEqual(target.read_text(), EARLIER_LAUNCHER)

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
        # A brace would end the ${NAME:-value} the launcher writes.
        brace_home = self.root / "home}x"
        brace_home.mkdir()
        self.fake_python(self.bin)
        for name, options, home in (
                ("python path", {"path": str(odd_python) + ":/usr/bin:/bin"}, self.home),
                ("home path", {"home": odd_home}, odd_home),
                ("home path with a brace", {"home": brace_home}, brace_home)):
            with self.subTest(name):
                done = self.install(**options)
                self.assertNotEqual(done.returncode, 0)
                self.assertIn("unsafe character", done.stderr)
                self.assertFalse((home / ".local").exists())

    def test_launcher_text_keeps_the_jar_line_and_names_absolute_defaults(self):
        python = self.fake_python(self.bin)
        self.assertEqual(self.install().returncode, 0)
        launcher = self.home / ".local/bin/trade"
        helper = self.home / ".local/lib/desk-keychain.py"
        self.assertTrue(os.access(launcher, os.X_OK))
        lines = launcher.read_text().splitlines()
        self.assertEqual(lines, [
            "#!/bin/sh",
            'export DESK_PYTHON="${DESK_PYTHON:-' + str(python) + '}"',
            'export DESK_KEY_HELPER="${DESK_KEY_HELPER:-' + str(helper) + '}"',
            'exec "${DESK_JAVA:-/opt/homebrew/opt/openjdk/bin/java}" '
            '-XX:+DisableAttachMechanism -jar "$HOME/.local/lib/desk.jar" "$@"'])

    def test_launcher_exports_the_defaults_and_passes_arguments_on(self):
        python = self.fake_python(self.bin)
        self.assertEqual(self.install().returncode, 0)
        helper = self.home / ".local/lib/desk-keychain.py"
        lines = self.launch("--keys")
        self.assertEqual(lines, [
            str(python), str(helper), "-XX:+DisableAttachMechanism", "-jar",
            str(self.home / ".local/lib/desk.jar"), "--keys"])
        self.assertTrue(all(os.path.isabs(line) for line in lines[:2]))

    def test_launcher_starts_java_with_attach_disabled_before_the_jar(self):
        self.fake_python(self.bin)
        self.assertEqual(self.install().returncode, 0)
        lines = self.launch("--keys")
        self.assertIn("-XX:+DisableAttachMechanism", lines)
        self.assertLess(lines.index("-XX:+DisableAttachMechanism"), lines.index("-jar"))
        self.assertEqual(lines.count("-XX:+DisableAttachMechanism"), 1)
        # A caller cannot drop the flag by setting a variable the launcher reads.
        lines = self.launch(DESK_PYTHON="/custom/python3", DESK_KEY_HELPER="/custom/helper.py")
        self.assertIn("-XX:+DisableAttachMechanism", lines)

    def test_the_launcher_records_the_interpreter_reported_by_python_itself(self):
        # command -v finds a shim or a link. The recorded path is what python reports.
        real = self.root / "real" / "bin" / "python3.12"
        shim = self.fake_python(self.bin, executable=real)
        self.assertNotEqual(shim, real)
        self.assertEqual(self.install().returncode, 0)
        text = (self.home / ".local/bin/trade").read_text()
        self.assertIn('export DESK_PYTHON="${DESK_PYTHON:-' + str(real) + '}"', text)
        self.assertNotIn(str(shim), text)
        self.assertEqual(self.launch("--keys")[0], str(real))
        # The libraries are still checked with the python that was found, and so is the path.
        self.assertEqual(self.args.read_text().splitlines(), [
            "-c", "import cryptography, Crypto, eth_account", "-c", "import sys; print(sys.executable)"])

    def test_a_reported_interpreter_that_is_not_an_absolute_path_stops_the_install(self):
        for name, reported in (("relative", "python3"), ("dot", "./python3"), ("empty", "")):
            with self.subTest(name):
                self.fake_python(self.bin, executable=reported)
                done = self.install()
                self.assertNotEqual(done.returncode, 0)
                self.assertIn("absolute", done.stderr)
                self.assertFalse((self.home / ".local").exists())

    def test_a_reported_interpreter_that_could_change_the_launcher_stops_the_install(self):
        for name, reported in (("dollar", "/opt/a$b/python3"), ("quote", '/opt/a"b/python3'),
                               ("backtick", "/opt/a`b/python3"), ("backslash", "/opt/a\\b/python3"),
                               ("brace", "/opt/a}b/python3")):
            with self.subTest(name):
                self.fake_python(self.bin, executable=reported)
                done = self.install()
                self.assertNotEqual(done.returncode, 0)
                self.assertIn("unsafe character", done.stderr)
                self.assertFalse((self.home / ".local").exists())

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

    def test_desk_vault_is_written_next_to_the_other_two_only_when_given(self):
        python = self.fake_python(self.bin)
        helper = self.home / ".local/lib/desk-keychain.py"
        launcher = self.home / ".local/bin/trade"
        for given in ((), ("DESK_VAULT=",)):
            with self.subTest(given=given):
                self.assertEqual(self.install(*given).returncode, 0)
                self.assertNotIn("DESK_VAULT", launcher.read_text())
        vault = self.root / "my vault"
        done = self.install("DESK_VAULT=" + str(vault))
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(launcher.read_text().splitlines(), [
            "#!/bin/sh",
            'export DESK_PYTHON="${DESK_PYTHON:-' + str(python) + '}"',
            'export DESK_KEY_HELPER="${DESK_KEY_HELPER:-' + str(helper) + '}"',
            'export DESK_VAULT="${DESK_VAULT:-' + str(vault) + '}"',
            'exec "${DESK_JAVA:-/opt/homebrew/opt/openjdk/bin/java}" '
            '-XX:+DisableAttachMechanism -jar "$HOME/.local/lib/desk.jar" "$@"'])
        # The launcher exports the recorded folder, and a value already set wins.
        java = self.root / "java"
        java.write_text("#!/bin/sh\nprintf '%s\\n' \"$DESK_VAULT\"\n")
        java.chmod(0o755)
        for variables, shown in (({}, str(vault)), ({"DESK_VAULT": "/custom/vault"}, "/custom/vault")):
            env = {"HOME": str(self.home), "PATH": "/usr/bin:/bin", "DESK_JAVA": str(java), **variables}
            ran = subprocess.run([str(launcher)], env=env, capture_output=True, text=True, timeout=60)
            self.assertEqual(ran.returncode, 0, ran.stderr)
            self.assertEqual(ran.stdout.splitlines(), [shown])

    def test_a_desk_vault_that_is_relative_or_could_change_the_launcher_stops_the_install(self):
        self.fake_python(self.bin)
        for name, value, reason in (
                ("relative", "vault", "absolute"), ("dot", "./vault", "absolute"),
                ("dollar", "/opt/a$$b", "unsafe character"), ("quote", '/opt/a"b', "unsafe character"),
                ("backtick", "/opt/a`b", "unsafe character"),
                ("backslash", "/opt/a\\b", "unsafe character"),
                ("brace", "/opt/a}b", "unsafe character")):
            with self.subTest(name):
                done = self.install("DESK_VAULT=" + value)
                self.assertNotEqual(done.returncode, 0)
                self.assertIn(reason, done.stderr)
                self.assertFalse((self.home / ".local").exists())


if __name__ == "__main__":
    unittest.main()
