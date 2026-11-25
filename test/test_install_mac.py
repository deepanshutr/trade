"""Runs tools/install-mac.sh with stand-ins, so nothing is installed or built.

brew, make, uname and Homebrew's python3 are stand-ins that record their calls, and HOME is a
temporary folder with a space in its name, so the real home gets no write and a missing quote
splits a path. The folder that holds the stand-ins comes first on PATH. The scratch Python
environment the install makes holds a python3 stand-in too. Each recorded call keeps every
argument in brackets and names the folder it ran in.
"""

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).parents[1]
BASH = shutil.which("bash")
INSTALL = ROOT / "tools/install-mac.sh"
MARKER = ("# Desk: ~/.local/bin at the end of PATH, for the trade command. "
          "Added by tools/install-mac.sh.")
PATH_LINE = ('case ":$PATH:" in *":$HOME/.local/bin:"*|*":$HOME/.local/bin/:"*) ;; '
             '*) PATH="${PATH:+$PATH:}$HOME/.local/bin" ;; esac')
ZSHRC_LINES = "\n" + MARKER + "\n" + PATH_LINE + "\n"
DONE = "Desk is installed. Open a new terminal, then type trade.\n"


def posix(path):
    """The path as bash reads it. Git Bash on Windows reads C:\\x as /c/x."""
    path = Path(path)
    if os.name != "nt":
        return str(path)
    return "/" + path.drive[0].lower() + path.as_posix()[2:]


def script(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("#!/bin/sh\n" + text, newline="\n")
    path.chmod(0o755)
    return path


def call(*words):
    """A recorded call as the stand-ins write it, run in the checkout."""
    return words[0] + "".join(" [" + word + "]" for word in words[1:]) + " in " + posix(ROOT)


class InstallMacTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if BASH is None:
            raise AssertionError("bash not found on PATH")

    def setUp(self):
        # The real home must come out of every test as it went in.
        real = [Path.home() / ".zshrc", Path.home() / ".local/lib/desk-venv"]
        before = [path.exists() for path in real]
        self.addCleanup(lambda: self.assertEqual([path.exists() for path in real], before,
                                                 "a test wrote into the real home"))
        temporary = tempfile.TemporaryDirectory(prefix="desk-install-mac-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.home = self.root / "the home"
        self.home.mkdir()
        self.log = self.root / "calls"
        self.bin = self.root / "stand-in"
        self.prefix = self.root / "brew prefix"
        self.java = self.prefix / "opt/openjdk@21"
        self.venv = self.home / ".local/lib/desk-venv"
        record = ("{ printf '%s' \"$(basename \"$0\")\"; for word in \"$@\"; do "
                  "printf ' [%s]' \"$word\"; done; printf ' in %s\\n' \"$(pwd)\"; } >> '"
                  + posix(self.log) + "'\n")
        script(self.bin / "uname", "printf '%s\\n' Darwin\n")
        script(self.bin / "brew", record + (
            "case \"$1 $2\" in\n"
            "  '--prefix openjdk@21') printf '%s\\n' '{java}';;\n"
            "  '--prefix ') printf '%s\\n' '{prefix}';;\n"
            "esac\n").format(java=posix(self.java), prefix=posix(self.prefix)))
        # Homebrew's python3 makes the Python environment, whose python3 records its calls.
        # Each stand-in that writes refuses a folder outside the scratch home, so a script that
        # lost its HOME cannot make one write outside it.
        inside = "case \"$1\" in '" + posix(self.home) + "'/*) ;; *) exit 9;; esac\n"
        venv_python = script(self.root / "venv-python", record)
        script(self.prefix / "bin/python3", record + (
            "if [ \"$1 $2\" = '-m venv' ]; then\n"
            "  ( set -- \"$3\"; {inside} ) || exit 9\n"
            "  mkdir -p \"$3/bin\" && cp '{venv_python}' \"$3/bin/python3\"\n"
            "fi\n").format(inside=inside.rstrip("\n"), venv_python=posix(venv_python)))
        # make install writes the launcher, as the real one does.
        script(self.bin / "make", record
               + "printf 'make PATH %s\\n' \"$PATH\" >> '" + posix(self.log) + "'\n"
               + "( set -- \"$HOME\"/x; " + inside.rstrip("\n") + " ) || exit 9\n"
               + "mkdir -p \"$HOME/.local/bin\" && : > \"$HOME/.local/bin/trade\"\n")
        self.path = posix(self.bin) + ":/usr/bin:/bin"

    def run_install(self, path=None, home=True):
        """Without a home, an outer bash unsets HOME, since Git Bash started from Windows with
        no HOME takes the account's own profile folder as HOME."""
        env = {"HOME": posix(self.home), "PATH": path or self.path}
        command = [BASH, posix(INSTALL)] if home else [
            BASH, "-c", "unset HOME; exec bash \"$0\"", posix(INSTALL)]
        return subprocess.run(command, env=env, cwd=self.root, capture_output=True, text=True,
                              timeout=60)

    def calls(self):
        return self.log.read_text().splitlines() if self.log.exists() else []

    def zshrc(self):
        zshrc = self.home / ".zshrc"
        return zshrc.read_text() if zshrc.exists() else None

    def expected_calls(self):
        venv = posix(self.venv)
        return [
            call("brew", "install", "openjdk@21", "scala-cli", "python"),
            call("brew", "--prefix", "openjdk@21"),
            call("brew", "--prefix"),
            call("python3", "-m", "venv", venv),
            call("python3", "-m", "pip", "install", "--quiet", "--requirement",
                 "tools/requirements.txt"),
            call("make", "install", "DESK_JAVA_HOME=" + posix(self.java)),
            "make PATH " + venv + "/bin:" + self.path,
        ]

    def assert_clean_success(self, done):
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(done.stdout, DONE)
        self.assertEqual(done.stderr, "")

    def test_installs_java_python_and_desk_in_order(self):
        self.assert_clean_success(self.run_install())
        self.assertEqual(self.calls(), self.expected_calls())

    def test_a_rerun_repeats_every_step(self):
        self.assert_clean_success(self.run_install())
        self.log.unlink()
        self.assert_clean_success(self.run_install())
        self.assertEqual(self.calls(), self.expected_calls())

    def test_puts_local_bin_on_path_in_zshrc_once(self):
        (self.home / ".zshrc").write_text("export EDITOR=vi\n")
        self.assert_clean_success(self.run_install())
        self.assert_clean_success(self.run_install())
        self.assertEqual(self.zshrc(), "export EDITOR=vi\n" + ZSHRC_LINES)

    def test_writes_a_new_zshrc_when_there_is_none(self):
        self.assert_clean_success(self.run_install())
        self.assertEqual(self.zshrc(), ZSHRC_LINES)

    def test_a_line_that_only_holds_the_marker_is_not_the_marker(self):
        for near in ("# was " + MARKER + "\n", MARKER + " Removed.\n"):
            with self.subTest(near=near):
                self.setUp()
                (self.home / ".zshrc").write_text(near)
                self.assert_clean_success(self.run_install())
                self.assertEqual(self.zshrc(), near + ZSHRC_LINES)

    def test_the_zshrc_lines_put_local_bin_last_on_path(self):
        self.assert_clean_success(self.run_install())
        shown = subprocess.run(
            [BASH, "-c", ". \"$HOME/.zshrc\"; printf '%s' \"$PATH\""],
            env={"HOME": posix(self.home), "PATH": "/usr/bin:/bin"},
            capture_output=True, text=True, timeout=60)
        self.assertEqual(shown.stdout, "/usr/bin:/bin:" + posix(self.home) + "/.local/bin")

    def test_leaves_zshrc_alone_when_local_bin_is_already_on_path(self):
        local = posix(self.home) + "/.local/bin"
        for path in (self.path + ":" + local, self.path + ":" + local + "/",
                     local + ":" + self.path, posix(self.bin) + ":" + local + ":/usr/bin:/bin"):
            with self.subTest(path=path):
                self.assert_clean_success(self.run_install(path))
                self.assertIsNone(self.zshrc())

    def test_a_lookalike_path_entry_is_not_local_bin(self):
        for suffix in ("/.local/bin2", "/.local/bin/sub", "/.local", "/.local/bin.old"):
            for prefix in ("", "/x"):
                with self.subTest(entry=prefix + "HOME" + suffix):
                    self.setUp()
                    entry = prefix + posix(self.home) + suffix
                    self.assert_clean_success(self.run_install(self.path + ":" + entry))
                    self.assertEqual(self.zshrc(), ZSHRC_LINES)

    def test_a_prefixed_local_bin_entry_is_not_local_bin(self):
        entry = "/x" + posix(self.home) + "/.local/bin"
        self.assert_clean_success(self.run_install(self.path + ":" + entry))
        self.assertEqual(self.zshrc(), ZSHRC_LINES)

    def test_refuses_another_system_before_anything(self):
        script(self.bin / "uname", "printf '%s\\n' Linux\n")
        done = self.run_install()
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("This install is for macOS", done.stderr)
        self.assertEqual(self.calls(), [])
        self.assertEqual(list(self.home.iterdir()), [])

    def test_names_homebrew_when_it_is_missing(self):
        (self.bin / "brew").unlink()
        done = self.run_install()
        self.assertNotEqual(done.returncode, 0)
        self.assertIn("https://brew.sh", done.stderr)
        self.assertEqual(self.calls(), [])
        self.assertEqual(list(self.home.iterdir()), [])

    def test_stops_without_a_home(self):
        done = self.run_install(home=False)
        self.assertNotEqual(done.returncode, 0)
        self.assertFalse(any(line.startswith(("python3", "make")) for line in self.calls()))
        self.assertNotIn(DONE, done.stdout)

    def test_a_failed_step_stops_the_install(self):
        steps = {
            "brew": ("brew", "case \"$1\" in install) exit 1;; esac\n"),
            "java prefix": ("brew", "case \"$1 $2\" in '--prefix openjdk@21') exit 1;; esac\n"),
            "brew prefix": ("brew", "case \"$1 $2\" in '--prefix ') exit 1;; esac\n"),
            "venv": ("python", "[ \"$2\" = venv ] && exit 1\n"),
            "pip": ("venv", "[ \"$2\" = pip ] && exit 1\n"),
            "make": ("make", "exit 1\n"),
        }
        for step, (where, failure) in steps.items():
            with self.subTest(step=step):
                self.setUp()
                target = {"brew": self.bin / "brew", "python": self.prefix / "bin/python3",
                          "venv": self.root / "venv-python", "make": self.bin / "make"}[where]
                text = target.read_text()
                target.write_text(text.replace("#!/bin/sh\n", "#!/bin/sh\n" + failure, 1),
                                  newline="\n")
                done = self.run_install()
                self.assertNotEqual(done.returncode, 0)
                self.assertNotIn(DONE, done.stdout)
                self.assertIsNone(self.zshrc())
                if step != "make":
                    self.assertFalse(any(line.startswith("make ") for line in self.calls()))

    def test_requirements_pin_the_key_helper_libraries(self):
        lines = [line for line in (ROOT / "tools/requirements.txt").read_text().splitlines()
                 if line and not line.startswith("#")]
        self.assertEqual(lines, ["cryptography==50.0.2", "pycryptodome==4.4.0",
                                 "eth-account==0.14.0"])


if __name__ == "__main__":
    unittest.main()
