"""Second, independent check of the agent key derivation.

This file recomputes the seed text, the Argon2id output, the scalar reduction and the
address in Python, and compares them with fixtures/agent-key-vectors.json. It never reads
or runs the Scala code. Run it with --write to regenerate the fixture file. Every seed and
passphrase here is a fixed test value.
"""

import functools
import hashlib
import importlib.util
import json
from pathlib import Path
import sys
import unittest

ROOT = Path(__file__).parents[1]
FIXTURE = ROOT / "fixtures/agent-key-vectors.json"

spec = importlib.util.spec_from_file_location("keychain_wallet", ROOT / "tools/keychain_wallet.py")
wallet = importlib.util.module_from_spec(spec)
try:
    spec.loader.exec_module(wallet)
except ModuleNotFoundError as error:
    if (error.name or "").split(".")[0] not in {"cryptography", "Crypto", "eth_account"}:
        raise
    raise unittest.SkipTest("key helper crypto module missing: " + error.name) from error

from cryptography.hazmat.primitives.kdf.argon2 import Argon2id

ALPHABET = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
ORDER = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
SALT_PREFIX = b"desk-agent-v1\x00"

# name, seed (32 bytes), passphrase
VECTORS = [
    ("counting seed, 8 byte passphrase", bytes(range(32)), "pass 123"),
    (
        "mixed seed, 128 byte passphrase",
        bytes.fromhex("0123456789abcdef" * 4),
        "".join(chr(32 + (index * 7) % 95) for index in range(128)),
    ),
    ("zero seed, plain passphrase", bytes(32), "fixture passphrase three"),
    ("all ones seed, full printable range", b"\xff" * 32, "".join(chr(c) for c in range(32, 127))),
    (
        "alternating seed, edge spaces",
        bytes.fromhex("a55a" * 16),
        "  spaces at both ends  ",
    ),
]

# name, 32 byte reduce input
REDUCES = [
    ("zero", bytes(32)),
    ("order itself", ORDER.to_bytes(32, "big")),
    ("all ones", b"\xff" * 32),
    ("one", (1).to_bytes(32, "big")),
    ("order minus one", (ORDER - 1).to_bytes(32, "big")),
]


def seed_text(seed):
    bits = format(int.from_bytes(seed, "big"), "0256b") + "0000"
    data = "".join(ALPHABET[int(bits[start:start + 5], 2)] for start in range(0, 260, 5))
    groups = []
    for index in range(13):
        chunk = data[index * 4:index * 4 + 4]
        digest = hashlib.sha256(b"desk-seed-v1" + bytes([index]) + chunk.encode("ascii")).digest()
        groups.append(chunk + ALPHABET[digest[0] >> 3])
    return "-".join(groups)


def reduce(output):
    if 0 < int.from_bytes(output, "big") < ORDER:
        return output, 0
    counter = 1
    while True:
        candidate = hashlib.sha256(output + counter.to_bytes(4, "big")).digest()
        if 0 < int.from_bytes(candidate, "big") < ORDER:
            return candidate, counter
        counter += 1


@functools.lru_cache(maxsize=None)
def argon2(seed, passphrase):
    kdf = Argon2id(salt=SALT_PREFIX + passphrase, length=32, iterations=3, lanes=4,
                   memory_cost=262144)
    return kdf.derive(seed)


def vector(name, seed, passphrase):
    output = argon2(seed, passphrase.encode("ascii"))
    key, counter = reduce(output)
    return {
        "name": name,
        "seed": seed.hex(),
        "passphrase": passphrase,
        "seedText": seed_text(seed),
        "output": output.hex(),
        "counter": counter,
        "key": key.hex(),
        "address": wallet.address(key),
    }


def reduce_case(name, value):
    key, counter = reduce(value)
    return {"name": name, "input": value.hex(), "counter": counter, "key": key.hex()}


def build():
    return {
        "vectors": [vector(*item) for item in VECTORS],
        "reduce": [reduce_case(*item) for item in REDUCES],
    }


class AgentKeyVectorTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.fixture = json.loads(FIXTURE.read_text())

    def test_the_order_matches_the_helper(self):
        self.assertEqual(ORDER, wallet.ORDER)

    def test_argon2id_matches_the_rfc_9106_section_5_3_vector(self):
        tag = Argon2id(
            salt=b"\x02" * 16, length=32, iterations=3, lanes=4, memory_cost=32,
            ad=b"\x04" * 12, secret=b"\x03" * 8,
        ).derive(b"\x01" * 32)
        self.assertEqual(
            tag.hex(), "0d640df58d78766c08c037a34a8b53c9d01ef0452d75b65eb52520e96b01e659"
        )

    def test_the_fixture_has_the_required_shape(self):
        self.assertEqual(set(self.fixture), {"vectors", "reduce"})
        self.assertEqual(len(self.fixture["vectors"]), 5)
        lengths = {len(item["passphrase"]) for item in self.fixture["vectors"]}
        self.assertIn(8, lengths)
        self.assertIn(128, lengths)
        names = {item["name"]: item["input"] for item in self.fixture["reduce"]}
        self.assertEqual(names["zero"], "00" * 32)
        self.assertEqual(names["order itself"], ORDER.to_bytes(32, "big").hex())
        self.assertEqual(names["all ones"], "ff" * 32)

    def test_every_fixture_vector_recomputes(self):
        for item in self.fixture["vectors"]:
            with self.subTest(vector=item["name"]):
                seed = bytes.fromhex(item["seed"])
                self.assertEqual(len(seed), 32)
                self.assertEqual(vector(item["name"], seed, item["passphrase"]), item)

    def test_every_fixture_reduce_case_recomputes(self):
        for item in self.fixture["reduce"]:
            with self.subTest(case=item["name"]):
                self.assertEqual(reduce_case(item["name"], bytes.fromhex(item["input"])), item)

    def test_the_out_of_range_cases_use_counter_one_and_the_others_none(self):
        counters = {item["name"]: item["counter"] for item in self.fixture["reduce"]}
        self.assertEqual(
            counters,
            {"zero": 1, "order itself": 1, "all ones": 1, "one": 0, "order minus one": 0},
        )
        for item in self.fixture["reduce"]:
            if item["counter"] == 1:
                expected = hashlib.sha256(bytes.fromhex(item["input"]) + b"\x00\x00\x00\x01")
                self.assertEqual(item["key"], expected.hexdigest())
            else:
                self.assertEqual(item["key"], item["input"])

    def test_the_fixture_file_is_what_this_code_generates(self):
        self.assertEqual(self.fixture, build())
        self.assertEqual(FIXTURE.read_text(), render(build()))

    def test_the_seed_text_has_the_documented_shape(self):
        for item in self.fixture["vectors"]:
            text = item["seedText"]
            self.assertEqual(len(text), 77)
            self.assertEqual(text.count("-"), 12)
            self.assertTrue(set(text.replace("-", "")) <= set(ALPHABET))
        self.assertEqual(seed_text(b"\xff" * 32).split("-")[12][:4], "ZZZG")


def render(content):
    return json.dumps(content, indent=2) + "\n"


if __name__ == "__main__":
    if sys.argv[1:] == ["--write"]:
        FIXTURE.write_text(render(build()))
    else:
        unittest.main()
