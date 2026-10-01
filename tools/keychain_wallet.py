"""Store a Desk trading key in the Mac Keychain without exporting it.

The key arrives on standard input as 64 lowercase hex characters. Only public
metadata reaches stdout. Python and its crypto libraries can retain temporary
immutable copies until this short process exits. This helper never replaces an
item, approves a trading key, or submits an order.
"""

import argparse
import ctypes as C
import json
import os
from pathlib import Path
import re
import sys

from cryptography.hazmat.primitives.asymmetric import ec
from Crypto.Hash import keccak
from eth_account import Account

ORDER = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
SERVICES = {"owner": "desk-owner", "agent": "desk"}
DUPLICATE_ITEM = -25299  # errSecDuplicateItem
KEY_INPUT = 66  # Up to 65 bytes are a key. A 66th byte means more input follows.


class SetupError(RuntimeError):
    pass


class DuplicateItem(SetupError):
    pass


def address(secret):
    scalar = int.from_bytes(secret, "big")
    if len(secret) != 32 or not 0 < scalar < ORDER:
        raise SetupError("invalid private scalar")
    public = ec.derive_private_key(scalar, ec.SECP256K1()).public_key().public_numbers()
    digest = keccak.new(digest_bits=256)
    digest.update(public.x.to_bytes(32, "big") + public.y.to_bytes(32, "big"))
    result = "0x" + digest.hexdigest()[-40:]
    if result != Account.from_key(bytes(secret)).address.lower():
        raise SetupError("independent public addresses differ")
    return result


def generate():
    key = ec.generate_private_key(ec.SECP256K1())
    return bytearray(key.private_numbers().private_value.to_bytes(32, "big"))


def hex_value(byte):
    if 48 <= byte <= 57:
        return byte - 48
    if 97 <= byte <= 102:
        return byte - 87
    raise SetupError("invalid key input")


def read_key(stream):
    """Reads 64 lowercase hex characters and an optional newline into a new buffer.

    Reads at most 66 bytes and refuses anything else. The key never becomes a str.
    """
    raw, key = bytearray(KEY_INPUT), bytearray(32)
    try:
        view, used = memoryview(raw), 0
        while used < KEY_INPUT:
            count = stream.readinto(view[used:])
            if not count:
                break
            used += count
        if used not in (64, 65) or (used == 65 and raw[64] != 10):
            raise SetupError("invalid key input")
        for index in range(32):
            key[index] = hex_value(raw[2 * index]) * 16 + hex_value(raw[2 * index + 1])
        return key
    except BaseException:
        key[:] = bytes(len(key))
        raise
    finally:
        raw[:] = bytes(len(raw))


def service_name(value):
    if not re.fullmatch(r"[A-Za-z0-9._-]{1,80}", value):
        raise argparse.ArgumentTypeError("service name must match [A-Za-z0-9._-]{1,80}")
    return value


def create(role, store, generate, before_store=lambda public: None, service=None):
    if role not in SERVICES:
        raise SetupError("unknown wallet role")
    secret = generate()
    encoded = bytearray()
    try:
        public = {"role": role, "service": service or SERVICES[role],
                  "account": address(secret)}
        before_store(public)
        encoded.extend(secret.hex().encode("ascii"))
        store.add(public["service"], public["account"], "Desk " + role + " key", encoded)
        return public
    finally:
        secret[:] = bytes(len(secret))
        encoded[:] = bytes(len(encoded))


def acceptable_read_status(status):
    return status == -25308  # errSecInteractionNotAllowed, not item missing.


class Attribute(C.Structure):
    _fields_ = [("tag", C.c_uint32), ("length", C.c_uint32), ("data", C.c_void_p)]


class Attributes(C.Structure):
    _fields_ = [("count", C.c_uint32), ("attr", C.POINTER(Attribute))]


def fourcc(value):
    return int.from_bytes(value.encode("ascii"), "big")


class MacKeychain:
    def __init__(self):
        if sys.platform != "darwin":
            raise SetupError("Mac key store required")
        root = "/System/Library/Frameworks/"
        self.cf = C.CDLL(root + "CoreFoundation.framework/CoreFoundation")
        self.sec = C.CDLL(root + "Security.framework/Security")
        pointer = C.c_void_p
        reference = C.POINTER(pointer)
        self._bind(self.cf, "CFStringCreateWithCString", pointer,
                   [pointer, C.c_char_p, C.c_uint32])
        self._bind(self.cf, "CFArrayCreate", pointer, [pointer, pointer, C.c_long, pointer])
        self._bind(self.cf, "CFArrayGetCount", C.c_long, [pointer])
        self._bind(self.cf, "CFArrayGetValueAtIndex", pointer, [pointer, C.c_long])
        self._bind(self.cf, "CFEqual", C.c_ubyte, [pointer, pointer])
        self._bind(self.cf, "CFRelease", None, [pointer])
        signatures = {
            "SecAccessCreate": [pointer, pointer, reference],
            "SecAccessCopyACLList": [pointer, reference],
            "SecACLCopyContents": [pointer, reference, reference, C.POINTER(C.c_uint16)],
            "SecACLSetContents": [pointer, pointer, pointer, C.c_uint16],
            "SecKeychainOpen": [C.c_char_p, reference],
            "SecKeychainItemCreateFromContent": [C.c_uint32, C.POINTER(Attributes),
                                                 C.c_uint32, pointer, pointer, pointer,
                                                 reference],
            "SecKeychainItemCopyAccess": [pointer, reference],
            "SecKeychainGetUserInteractionAllowed": [C.POINTER(C.c_ubyte)],
            "SecKeychainSetUserInteractionAllowed": [C.c_ubyte],
            "SecKeychainItemCopyContent": [pointer, pointer, pointer,
                                           C.POINTER(C.c_uint32), reference],
            "SecKeychainItemFreeContent": [pointer, pointer],
        }
        for name, arguments in signatures.items():
            self._bind(self.sec, name, C.c_int32, arguments)
        self._bind(self.sec, "SecACLCopyAuthorizations", pointer, [pointer])
        self.read_tags = [C.c_void_p.in_dll(self.sec, name).value for name in
                          ("kSecACLAuthorizationDecrypt", "kSecACLAuthorizationAny")]

    @staticmethod
    def _bind(library, name, returns, arguments):
        function = getattr(library, name)
        function.restype, function.argtypes = returns, arguments

    @staticmethod
    def check(status):
        if status != 0:
            raise SetupError("key store status " + str(status))

    def release(self, *references):
        for reference in references:
            if reference:
                self.cf.CFRelease(reference)

    def values(self, array):
        return [self.cf.CFArrayGetValueAtIndex(array, i)
                for i in range(self.cf.CFArrayGetCount(array))]

    def access_rules(self, access, configure=False):
        entries = C.c_void_p()
        self.check(self.sec.SecAccessCopyACLList(access, C.byref(entries)))
        count = 0
        try:
            for entry in self.values(entries):
                tags = self.sec.SecACLCopyAuthorizations(entry)
                if not tags:
                    raise SetupError("access authorizations unavailable")
                try:
                    reads = any(self.cf.CFEqual(tag, needed)
                                for tag in self.values(tags) for needed in self.read_tags)
                finally:
                    self.release(tags)
                if not reads:
                    continue
                apps, description, flags = C.c_void_p(), C.c_void_p(), C.c_uint16()
                self.check(self.sec.SecACLCopyContents(
                    entry, C.byref(apps), C.byref(description), C.byref(flags)))
                try:
                    if not apps or self.cf.CFArrayGetCount(apps) != 0:
                        raise SetupError("a key reader is preauthorized")
                    if configure:
                        self.check(self.sec.SecACLSetContents(
                            entry, apps, description, flags.value | 1))
                    elif not flags.value & 1:
                        raise SetupError("password prompt is not required")
                    count += 1
                finally:
                    self.release(apps, description)
            if count == 0:
                raise SetupError("no secret read authorization found")
            return count
        finally:
            self.release(entries)

    def access(self, label):
        title = self.cf.CFStringCreateWithCString(None, label.encode("utf-8"), 0x08000100)
        empty = self.cf.CFArrayCreate(None, None, 0, None)
        access = C.c_void_p()
        try:
            if not title or not empty:
                raise SetupError("access allocation failed")
            self.check(self.sec.SecAccessCreate(title, empty, C.byref(access)))
            self.access_rules(access, configure=True)
            self.access_rules(access)
            return access
        except BaseException:
            self.release(access)
            raise
        finally:
            self.release(title, empty)

    def probe(self, item):
        allowed, size, data = C.c_ubyte(), C.c_uint32(), C.c_void_p()
        self.check(self.sec.SecKeychainGetUserInteractionAllowed(C.byref(allowed)))
        self.check(self.sec.SecKeychainSetUserInteractionAllowed(False))
        try:
            status = self.sec.SecKeychainItemCopyContent(
                item, None, None, C.byref(size), C.byref(data))
            if not acceptable_read_status(status):
                raise SetupError("new item did not refuse access without a prompt")
        finally:
            if data:
                C.memset(data, 0, size.value)
                self.sec.SecKeychainItemFreeContent(None, data)
            self.check(self.sec.SecKeychainSetUserInteractionAllowed(allowed))

    def add(self, service, account, label, secret):
        access = self.access(label)
        keychain, item, saved_access = C.c_void_p(), C.c_void_p(), C.c_void_p()
        buffers = [C.create_string_buffer(value.encode("utf-8"))
                   for value in (service, account, label)]
        attributes = (Attribute * 3)(*(
            Attribute(fourcc(tag), len(buffer.value), C.cast(buffer, C.c_void_p))
            for tag, buffer in zip(("svce", "acct", "labl"), buffers)))
        listing = Attributes(3, attributes)
        raw = (C.c_ubyte * len(secret)).from_buffer(secret)
        try:
            path = Path.home() / "Library/Keychains/login.keychain-db"
            self.check(self.sec.SecKeychainOpen(os.fsencode(path), C.byref(keychain)))
            # Create-only. No update or delete function is bound by this helper.
            status = self.sec.SecKeychainItemCreateFromContent(
                fourcc("genp"), C.byref(listing), len(secret), raw,
                keychain, access, C.byref(item))
            if status == DUPLICATE_ITEM:
                raise DuplicateItem("key store status " + str(status))
            self.check(status)
            self.check(self.sec.SecKeychainItemCopyAccess(item, C.byref(saved_access)))
            self.access_rules(saved_access)
            self.probe(item)
        finally:
            self.release(saved_access, item, keychain, access)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    actions = parser.add_mutually_exclusive_group(required=True)
    actions.add_argument("--check-access", action="store_true",
                         help="check native access objects without creating any key")
    actions.add_argument("--store", choices=["agent"],
                         help="store the key read on standard input")
    parser.add_argument("--service", type=service_name,
                        help="Keychain service name, default " + SERVICES["agent"])
    args = parser.parse_args()
    public = None
    key = None
    try:
        if args.check_access:
            store = MacKeychain()
            access = store.access("Desk access preparation")
            try:
                print(json.dumps({"read_rules": store.access_rules(access),
                                  "password_required": True, "trusted_readers": 0,
                                  "keychain_items_created": 0}))
            finally:
                store.release(access)
            return 0
        key = read_key(sys.stdin.buffer)
        store = MacKeychain()

        def keep(value):
            nonlocal public
            public = dict(value)

        public = create(args.store, store, lambda: key, keep, args.service)
        print(json.dumps({"account": public["account"], "role": public["role"],
                          "service": public["service"], "state": "stored",
                          "prompt_required": True}, separators=(",", ":")))
        return 0
    except DuplicateItem:
        print(json.dumps({"account": public["account"], "state": "exists"},
                         separators=(",", ":")))
        return 3
    except Exception as error:
        stopped = {"state": "stopped", "item": public,
                   "message": "Setup did not finish. Preserve any setup record and inspect the named item before retrying."}
        if isinstance(error, SetupError):
            stopped["reason"] = str(error)
        print(json.dumps(stopped), file=sys.stderr)
        return 1
    finally:
        if key is not None:
            key[:] = bytes(len(key))


if __name__ == "__main__":
    raise SystemExit(main())
