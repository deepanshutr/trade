"""Runs the Makefile's check, test, build and install steps with stand-ins, so nothing builds.

make runs from a temporary folder with -f on the repository's Makefile. SCALA_CLI is a stand-in
that records its arguments, python3 is a stand-in that passes the library check, uname is a
stand-in that prints the system a test picks, and HOME is a temporary folder, so the checkout and
the real home get no write. The folder that holds the stand-in uname comes first on every PATH,
so the real uname never answers.
"""

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).parents[1]
MAKE = shutil.which("make")
BASH = shutil.which("bash")
REAL_HOME = os.path.expanduser("~")
# Each target that compiles or runs Desk, and the word that names its scala-cli call.
SCALA_TARGETS = {"check": "compile", "test": "test", "live": "test", "slow": "test",
                 "build": "package"}
TARGETS = (*SCALA_TARGETS, "install-launcher", "install")
# What uname -s prints on the Mac, and under Git Bash on Windows.
OTHER_SYSTEMS = ("Darwin", "MINGW64_NT-10.0")
HOMEBREW_JAVA = "/opt/homebrew/opt/openjdk/bin/java"
# The ~/.bashrc a new user gets on a common Linux system.
DEFAULT_BASHRC = ("#\n# ~/.bashrc\n#\n\n# If not running interactively, don't do anything\n"
                  "[[ $- != *i* ]] && return\n")
# The two lines the install adds to ~/.bashrc on Linux, after a newline.
PATH_LINES = (
    "# Desk: ~/.local/bin at the end of PATH, for the trade command. Added by make install.\n"
    'case ":$PATH:" in *":$HOME/.local/bin:"*|*":$HOME/.local/bin/:"*) ;; '
    '*) PATH="${PATH:+$PATH:}$HOME/.local/bin" ;; esac\n')
ADDED = "Added ~/.local/bin to the end of PATH in ~/.bashrc. Open a new terminal, then type trade."


def script(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("#!/bin/sh\n" + text)
    path.chmod(0o755)
    return path


def java_home(folder):
    """A Java home whose bin/java prints its own path and its arguments."""
    script(folder / "bin/java", "printf '%s\\n' \"$0\" \"$@\"\n")
    return folder


class MakefileTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        for name, found in (("make", MAKE), ("bash", BASH)):
            if found is None:
                raise AssertionError(name + " not found on PATH. Put the folder that holds it, "
                                     "such as ~/.local/bin, on PATH.")

    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="desk-make-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.work = self.root / "work"
        self.work.mkdir()
        shutil.copytree(ROOT / "tools", self.work / "tools")
        # A file named like the step, such as a link to a Java, must not let make skip the checks.
        (self.work / "java-home").write_text("")
        self.home = self.root / "home"
        self.home.mkdir()
        self.calls = self.root / "scala-cli-calls"
        # One line per call, each argument followed by a tab. A package call gets its jar.
        self.scala = script(self.root / "stand-in/scala-cli", (
            "for word in \"$@\"; do printf '%s\\t' \"$word\"; done >> '{calls}'\n"
            "printf '\\n' >> '{calls}'\n"
            "while [ $# -gt 1 ]; do\n"
            "  if [ \"$1\" = -o ]; then printf 'new jar' > \"$2\"; fi\n"
            "  shift\n"
            "done\n").format(calls=self.calls))
        self.uname = self.root / "uname/uname"
        self.python(fails_import=False)
        self.path = ":".join((str(self.uname.parent), str(self.root / "python"), "/usr/bin:/bin"))
        # A space in the folder shows the shell keeps it one argument.
        self.java_home = java_home(self.root / "java home")

    def python(self, fails_import):
        """A python3 that prints its own path when asked for sys.executable and passes every
        other call. When told, it fails the library import alone, so only the import check can
        stop the install."""
        python = self.root / "python/python3"
        script(python,
               "case \"$2\" in *sys.executable*) printf '%s\\n' '" + str(python) + "';; esac\n"
               + ("case \"$2\" in 'import cryptography'*) exit 1;; esac\n" if fails_import
                  else ""))

    def make(self, *arguments, java=None, system="Linux", uname=""):
        """Runs make with uname -s printing system. uname fails when system is None, and
        for any arguments but -s alone. Text in uname runs first in the stand-in."""
        script(self.uname, "exit 1\n" if system is None else
               "[ \"$#\" = 1 ] && [ \"$1\" = -s ] || exit 1\n" + uname
               + "printf '%s\\n' '" + system + "'\n")
        command = [MAKE, "-f", str(ROOT / "Makefile"), *arguments,
                   "SCALA_CLI=" + str(self.scala), "HOME=" + str(self.home)]
        if java is not None:
            command.append("DESK_JAVA_HOME=" + str(java).replace("$", "$$"))
        env = {"HOME": str(self.home), "PATH": self.path}
        # A dry run only prints the recipes. One that names the real home stops the test.
        dry = subprocess.run([MAKE, "-n", *command[1:]], cwd=self.work, env=env,
                             capture_output=True, text=True, timeout=60)
        self.assertEqual(dry.returncode, 0, dry.stderr)
        self.assertNotIn(REAL_HOME, dry.stdout.replace(str(self.root), ""))
        return subprocess.run(command, cwd=self.work, env=env, capture_output=True, text=True,
                              timeout=60)

    def interactive_bash(self, command, path):
        """A bash that reads ~/.bashrc as Konsole's does, in the scratch home with no terminal."""
        done = subprocess.run([BASH, "-i", "-c", command],
                              env={"HOME": str(self.home), "PATH": path},
                              stdin=subprocess.DEVNULL, capture_output=True, text=True,
                              timeout=60, start_new_session=True)
        self.assertEqual(done.returncode, 0, done.stderr)
        return done.stdout.splitlines()

    def scala_calls(self):
        if not self.calls.exists():
            return []
        return [line.split("\t")[:-1] for line in self.calls.read_text().splitlines()]

    def reset(self):
        self.calls.unlink(missing_ok=True)
        shutil.rmtree(self.work / "bin", ignore_errors=True)
        shutil.rmtree(self.home)
        self.home.mkdir()

    def assert_stopped_before_anything(self, done, reason):
        self.assertNotEqual(done.returncode, 0, "make ran the target")
        self.assertIn(reason, done.stderr)
        self.assertFalse(self.calls.exists())
        self.assertFalse((self.work / "bin").exists())
        self.assertEqual(list(self.home.iterdir()), [])

    def launcher_java(self):
        return (self.home / ".local/bin/trade").read_text().splitlines()[-1]

    # A1
    def test_every_call_that_compiles_or_runs_has_no_build_server_and_the_given_java(self):
        for system in ("Linux", *OTHER_SYSTEMS):
            for target, word in SCALA_TARGETS.items():
                with self.subTest(target, system=system):
                    self.reset()
                    done = self.make(target, java=self.java_home, system=system)
                    self.assertEqual(done.returncode, 0, done.stderr)
                    [call] = [call for call in self.scala_calls() if word in call[:2]]
                    # Words after -- go to the test framework, not to scala-cli.
                    options = call[:call.index("--")] if "--" in call else call
                    self.assertIn("--server=false", options)
                    self.assertIn("--java-home", options)
                    self.assertEqual(options[options.index("--java-home") + 1],
                                     str(self.java_home))

    def bad_java_homes(self):
        """Each value is refused by one check alone. The other checks would pass it."""
        java_home(self.work / "jdk")
        unsafe = [java_home(self.root / ("a" + character + "b")) for character in "$\"`\\}"]
        empty = self.root / "empty"
        empty.mkdir()
        not_executable = java_home(self.root / "not executable")
        (not_executable / "bin/java").chmod(0o644)
        folder = self.root / "folder"
        (folder / "bin/java").mkdir(parents=True)
        return [("jdk", "absolute"), ("./jdk", "absolute"),
                *[(str(each), "unsafe character") for each in unsafe],
                (str(empty), "bin/java"), (str(not_executable), "bin/java"),
                (str(folder), "bin/java")]

    # A2
    def test_a_bad_java_home_stops_every_target_before_scala_cli_and_any_write(self):
        # install-launcher runs alone too, so its own prerequisite is tested apart from build's.
        bad = self.bad_java_homes()
        for system in ("Linux", "Darwin"):
            for value, reason in bad:
                for target in TARGETS:
                    with self.subTest(target, value=value, system=system):
                        self.reset()
                        done = self.make(target, java=value, system=system)
                        self.assert_stopped_before_anything(done, reason)

    # A3
    def test_off_linux_without_a_java_home_every_target_runs_and_no_call_names_a_java(self):
        for system in OTHER_SYSTEMS:
            for given in (None, ""):
                for target in TARGETS:
                    with self.subTest(target, given=given, system=system):
                        self.reset()
                        done = self.make(target, java=given, system=system)
                        self.assertEqual(done.returncode, 0, done.stderr)
                        calls = self.scala_calls()
                        self.assertEqual(bool(calls), target != "install-launcher")
                        self.assertFalse(any("--java-home" in call for call in calls))

    # A4, and on the Mac and under Git Bash too
    def test_the_launcher_starts_the_java_the_install_was_given_on_every_system(self):
        java = self.java_home / "bin/java"
        for system in ("Linux", *OTHER_SYSTEMS):
            with self.subTest(system=system):
                self.reset()
                done = self.make("install-launcher", java=self.java_home, system=system)
                self.assertEqual(done.returncode, 0, done.stderr)
                self.assertEqual(self.launcher_java(), 'exec "${DESK_JAVA:-' + str(java) + '}" '
                                 '-XX:+DisableAttachMechanism -jar "$HOME/.local/lib/desk.jar" "$@"')
                ran = subprocess.run([str(self.home / ".local/bin/trade"), "XMR", "--mute"],
                                     env={"HOME": str(self.home), "PATH": "/usr/bin:/bin"},
                                     capture_output=True, text=True, timeout=60)
                self.assertEqual(ran.returncode, 0, ran.stderr)
                self.assertEqual(ran.stdout.splitlines(), [
                    str(java), "-XX:+DisableAttachMechanism", "-jar",
                    str(self.home / ".local/lib/desk.jar"), "XMR", "--mute"])

    # A5
    def test_on_linux_without_a_java_home_every_target_stops_before_scala_cli_and_any_write(self):
        for given in (None, ""):
            for target in TARGETS:
                with self.subTest(target, given=given):
                    self.reset()
                    done = self.make(target, java=given, system="Linux")
                    self.assert_stopped_before_anything(done, "DESK_JAVA_HOME")

    # A6
    def test_off_linux_the_launcher_keeps_homebrew_java_and_no_shell_file_changes(self):
        bashrc = self.home / ".bashrc"
        for system in OTHER_SYSTEMS:
            for target in ("install-launcher", "install"):
                for earlier in (None, DEFAULT_BASHRC):
                    with self.subTest(target, system=system, earlier=bool(earlier)):
                        self.reset()
                        if earlier:
                            bashrc.write_text(earlier)
                        done = self.make(target, system=system)
                        self.assertEqual(done.returncode, 0, done.stderr)
                        self.assertIn('exec "${DESK_JAVA:-' + HOMEBREW_JAVA + '}" ',
                                      self.launcher_java())
                        if earlier:
                            self.assertEqual(bashrc.read_text(), earlier)
                        else:
                            self.assertFalse(bashrc.exists())
                        self.assertNotIn(ADDED, done.stdout.splitlines())

    # A7
    def test_install_renames_the_new_jar_into_place_so_an_open_reader_keeps_the_old_bytes(self):
        jar = self.home / ".local/lib/desk.jar"
        done = self.make("install", java=self.java_home)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(jar.read_bytes(), b"new jar")
        jar.write_bytes(b"old jar")
        with open(jar, "rb", buffering=0) as reader:
            done = self.make("install", java=self.java_home)
            self.assertEqual(done.returncode, 0, done.stderr)
            self.assertEqual(reader.read(), b"old jar")
        self.assertEqual(jar.read_bytes(), b"new jar")
        self.assertEqual(sorted(each.name for each in jar.parent.iterdir()),
                         ["desk-keychain.py", "desk.jar"])

    def assert_trade_is_found_last_on_path(self):
        """A new interactive bash finds the launcher once, and a system command of the same
        name wins over one in ~/.local/bin."""
        folder = self.home / ".local/bin"
        lines = self.interactive_bash('type -a trade; type -P ls; echo "$PATH"', "/usr/bin:/bin")
        self.assertEqual(len(lines), 3, lines)
        self.assertEqual(lines[0], "trade is " + str(folder / "trade"))
        self.assertFalse(lines[1].startswith(str(self.home)), lines[1])
        self.assertEqual(lines[2], "/usr/bin:/bin:" + str(folder))

    # A8
    def test_on_linux_the_install_puts_local_bin_last_on_path_in_bashrc_once(self):
        bashrc = self.home / ".bashrc"
        bashrc.write_text(DEFAULT_BASHRC)
        script(self.home / ".local/bin/ls", "echo stand-in ls\n")
        done = self.make("install-launcher", java=self.java_home)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(bashrc.read_text(), DEFAULT_BASHRC + "\n" + PATH_LINES)
        self.assert_trade_is_found_last_on_path()
        # A bash started from that bash adds nothing, so trade is still found once.
        nested = self.interactive_bash("bash -i -c 'type -a trade'", "/usr/bin:/bin")
        self.assertEqual(nested, ["trade is " + str(self.home / ".local/bin/trade")])
        # A PATH that already holds the folder stays as it is.
        held = "/usr/bin:" + str(self.home / ".local/bin") + ":/bin"
        self.assertEqual(self.interactive_bash('echo "$PATH"', held), [held])
        self.assertEqual(done.stdout.splitlines().count(ADDED), 1)

    # A9
    def test_a_second_install_leaves_bashrc_as_it_was(self):
        bashrc = self.home / ".bashrc"
        bashrc.write_text(DEFAULT_BASHRC)
        done = self.make("install-launcher", java=self.java_home)
        self.assertEqual(done.returncode, 0, done.stderr)
        first = bashrc.read_bytes()
        done = self.make("install-launcher", java=self.java_home)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(bashrc.read_bytes(), first)
        self.assertNotIn(ADDED, done.stdout.splitlines())

    # A9
    def test_a_bashrc_with_no_final_newline_keeps_its_last_line(self):
        bashrc = self.home / ".bashrc"
        bashrc.write_text("export EDITOR=vi")
        done = self.make("install-launcher", java=self.java_home)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(bashrc.read_text(), "export EDITOR=vi\n" + PATH_LINES)

    # A9
    def test_with_no_bashrc_the_install_makes_one(self):
        bashrc = self.home / ".bashrc"
        done = self.make("install-launcher", java=self.java_home)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertTrue(bashrc.is_file(), "no .bashrc was made")
        self.assertEqual(bashrc.read_text(), "\n" + PATH_LINES)
        self.assert_trade_is_found_last_on_path()

    # A10
    def test_a_failed_library_check_leaves_bashrc_as_it_was(self):
        bashrc = self.home / ".bashrc"
        bashrc.write_text(DEFAULT_BASHRC)
        self.python(fails_import=True)
        done = self.make("install-launcher", java=self.java_home)
        self.assertNotEqual(done.returncode, 0)
        self.assertEqual(bashrc.read_text(), DEFAULT_BASHRC)
        self.assertNotIn(ADDED, done.stdout.splitlines())

    def test_a_launcher_that_cannot_be_written_leaves_bashrc_as_it_was(self):
        # A folder named trade.new makes the launcher write fail after every check.
        bashrc = self.home / ".bashrc"
        bashrc.write_text(DEFAULT_BASHRC)
        (self.home / ".local/bin/trade.new").mkdir(parents=True)
        done = self.make("install-launcher", java=self.java_home)
        self.assertNotEqual(done.returncode, 0)
        self.assertEqual(bashrc.read_text(), DEFAULT_BASHRC)
        self.assertNotIn(ADDED, done.stdout.splitlines())

    def test_only_a_line_equal_to_the_first_line_counts_as_present(self):
        # One line holds the first line inside it. The other matches it only if dots read
        # as any character.
        marker = PATH_LINES.splitlines()[0]
        self.assertEqual(marker.count("command."), 1)
        earlier = "#" + marker + "\n" + marker.replace("command.", "command!") + "\n"
        bashrc = self.home / ".bashrc"
        bashrc.write_text(earlier)
        done = self.make("install-launcher", java=self.java_home)
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertEqual(bashrc.read_text(), earlier + "\n" + PATH_LINES)

    def test_a_missing_path_lines_file_stops_the_install_before_any_write(self):
        (self.work / "tools/bashrc-path.sh").unlink()
        done = self.make("install-launcher", java=self.java_home)
        self.assertNotEqual(done.returncode, 0)
        self.assertEqual(list(self.home.iterdir()), [])

    def test_a_read_only_bashrc_that_needs_nothing_passes(self):
        # Off Linux no line is due, and on Linux the lines are already there.
        bashrc = self.home / ".bashrc"
        for system, text in (("Darwin", DEFAULT_BASHRC), ("MINGW64_NT-10.0", DEFAULT_BASHRC),
                             ("Linux", DEFAULT_BASHRC + "\n" + PATH_LINES)):
            with self.subTest(system=system):
                self.reset()
                bashrc.write_text(text)
                bashrc.chmod(0o444)
                done = self.make("install-launcher", java=self.java_home, system=system)
                self.assertEqual(done.returncode, 0, done.stderr)
                self.assertEqual(bashrc.read_text(), text)
                self.assertTrue((self.home / ".local/bin/trade").is_file())

    def test_a_failed_uname_in_the_append_step_fails_the_install(self):
        # uname answers for the java-home step and the first ~/.bashrc step, then fails, so
        # the append step must stop on its own read.
        calls = self.root / "uname-calls"
        calls.mkdir()
        counted = ("[ -e '{c}/2' ] && exit 1\n"
                   "if [ -e '{c}/1' ]; then : > '{c}/2'; else : > '{c}/1'; fi\n").format(c=calls)
        done = self.make("install", java=self.java_home, uname=counted)
        self.assertNotEqual(done.returncode, 0, "make ran the target")
        self.assertTrue((calls / "2").exists())
        self.assertTrue((self.home / ".local/bin/trade").is_file())
        self.assertFalse((self.home / ".bashrc").exists())
        self.assertFalse((self.home / ".local/lib/desk.jar").exists())

    def test_a_bashrc_that_cannot_be_read_stops_the_install_before_any_write(self):
        # Its lines would stay hidden, so every install would add them again.
        bashrc = self.home / ".bashrc"
        bashrc.write_text(DEFAULT_BASHRC)
        bashrc.chmod(0o200)
        done = self.make("install", java=self.java_home)
        bashrc.chmod(0o644)
        self.assertNotEqual(done.returncode, 0, "make ran the target")
        self.assertIn(".bashrc cannot be read or written", done.stderr)
        self.assertEqual([each.name for each in self.home.iterdir()], [".bashrc"])
        self.assertEqual(bashrc.read_text(), DEFAULT_BASHRC)

    def test_a_bashrc_that_cannot_be_written_stops_the_install_before_any_write(self):
        bashrc = self.home / ".bashrc"
        folder = self.home / ".local/bin"

        def locked_home():
            # A home that cannot take a new file, though the launcher's folder could.
            folder.mkdir(parents=True)
            self.home.chmod(0o555)
            self.addCleanup(self.home.chmod, 0o755)

        cases = (("read only", lambda: (bashrc.write_text(DEFAULT_BASHRC), bashrc.chmod(0o444))),
                 ("a link to a missing file", lambda: bashrc.symlink_to(self.root / "gone/bashrc")),
                 ("a folder", bashrc.mkdir),
                 ("missing from a home that cannot be written", locked_home))
        for name, make in cases:
            with self.subTest(name):
                self.home.chmod(0o755)
                self.reset()
                make()
                before = sorted(each.name for each in self.home.rglob("*"))
                done = self.make("install-launcher", java=self.java_home)
                self.assertNotEqual(done.returncode, 0, "make ran the target")
                self.assertIn(".bashrc cannot be read or written", done.stderr)
                self.assertEqual(sorted(each.name for each in self.home.rglob("*")), before)
                if name == "read only":
                    self.assertEqual(bashrc.read_text(), DEFAULT_BASHRC)
                self.assertNotIn(ADDED, done.stdout.splitlines())
        self.home.chmod(0o755)

    def test_off_linux_make_prints_no_java_home_step_and_no_bashrc_text(self):
        for system in OTHER_SYSTEMS:
            step = self.make("-n", "java-home", system=system).stdout.splitlines()
            self.assertTrue(step)
            for target in TARGETS:
                with self.subTest(target, system=system):
                    self.reset()
                    done = self.make(target, system=system)
                    self.assertEqual(done.returncode, 0, done.stderr)
                    shown = done.stdout.splitlines()
                    self.assertEqual([line for line in step if line in shown], [])
                    self.assertNotIn(".bashrc", done.stdout)

    # A12
    def test_the_path_lines_add_no_empty_entry_and_see_a_trailing_slash(self):
        folder = str(self.home / ".local/bin")
        # A folder whose name only starts or ends like ~/.local/bin is another folder.
        slash, longer, inside = ("/usr/bin:" + folder + end + ":/bin" for end in ("/", "x", "/x"))
        outside = "/usr/bin:/mnt" + folder + ":/bin"
        for given, expected in (("", folder), (slash, slash), (longer, longer + ":" + folder),
                                (inside, inside + ":" + folder), (outside, outside + ":" + folder)):
            with self.subTest(given=given):
                done = subprocess.run(
                    ["/bin/sh", "-c", '. "$1" && printf "%s\\n" "$PATH"', "sh",
                     str(ROOT / "tools/bashrc-path.sh")],
                    env={"HOME": str(self.home), "PATH": given},
                    capture_output=True, text=True, timeout=60)
                self.assertEqual(done.returncode, 0, done.stderr)
                self.assertEqual(done.stdout, expected + "\n")

    # A13
    def test_a_failed_uname_stops_every_target_before_scala_cli_and_any_write(self):
        for target in TARGETS:
            with self.subTest(target):
                self.reset()
                done = self.make(target, java=self.java_home, system=None)
                self.assert_stopped_before_anything(done, "uname")


if __name__ == "__main__":
    unittest.main()
