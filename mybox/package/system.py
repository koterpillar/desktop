import json
import shutil
from abc import ABCMeta, abstractmethod
from functools import cache
from typing import Optional

from ..utils import *
from .base import Package


class Installer(metaclass=ABCMeta):
    @abstractmethod
    def install(self, *packages: str) -> None:
        pass

    def is_installed(self, package: str) -> bool:
        return self.installed_version(package) is not None

    @abstractmethod
    def installed_version(self, package: str) -> Optional[str]:
        pass

    @abstractmethod
    def latest_version(self, package: str) -> str:
        pass


class Brew(Installer):
    def install(self, *packages: str) -> None:
        run("brew", "reinstall", *packages)

    @cache
    def info(self, package: str) -> tuple[str, Optional[str]]:
        info = json.loads(run_output("brew", "info", "--json=v2", package))
        if info["casks"]:
            cask = info["casks"][0]
            return cask["version"], cask["installed"]
        if info["formulae"]:
            formula = info["formulae"][0]
            try:
                installed = formula["installed"][0]["version"]
            except IndexError:
                installed = None
            version = formula["versions"]["stable"]
            revision = formula.get("revision")
            if revision:
                version += f"_{revision}"
            return version, installed
        raise ValueError(f"Unexpected output from brew: {info}")

    def installed_version(self, package: str) -> Optional[str]:
        try:
            return self.info(package)[1]
        except IndexError:
            return None

    def latest_version(self, package: str) -> str:
        return self.info(package)[0]


class DNF(Installer):
    def install(self, *packages: str) -> None:
        run("sudo", "dnf", "install", "-y", *packages)
        run("sudo", "dnf", "upgrade", "-y", *packages)

    def installed_version(self, package: str) -> Optional[str]:
        output = run_output(
            "rpm", "--query", "--queryformat", "%{VERSION}", "--whatprovides", package
        ).strip()
        if not output:
            return None
        return output

    def latest_version(self, package: str) -> str:
        output = run_output(
            "dnf",
            "--quiet",
            "repoquery",
            "--queryformat",
            "%{VERSION}",
            "--latest-limit",
            "1",
            "--arch",
            "x86_64,noarch",
            "--whatprovides",
            package,
        ).strip()
        if not output or "\n" in output:
            raise Exception(f"Cannot determine version for {package}.")
        return output


class Apt(Installer):
    def install(self, *packages: str) -> None:
        run("sudo", "apt", "install", "--yes", *packages)

    def latest_version(self, package: str) -> str:
        output = run_output(
            "apt-cache", "show", "--quiet", "--no-all-versions", package
        ).strip()
        for line in output.splitlines():
            line = line.strip()
            if line.startswith("Version:"):
                return line.split(": ", 1)[-1]
        raise Exception(f"Cannot determine version for {package}.")

    def installed_version(self, package: str) -> Optional[str]:
        try:
            return run_output(
                "dpkg-query", "--showformat", "${Version}", "--show", package
            ).strip()
        except subprocess.CalledProcessError:
            return None


def linux_installer() -> Installer:
    if shutil.which("dnf"):
        return DNF()
    elif shutil.which("apt"):
        return Apt()
    else:
        raise NotImplementedError("Cannot find a package manager.")


INSTALLER = with_os(linux=linux_installer, macos=lambda: Brew())()


class SystemPackage(Package):
    services: list[str]

    def __init__(
        self,
        *,
        name: str,
        auto_updates: bool = False,
        service: Some[str] = None,
        **kwargs,
    ) -> None:
        self._name = name
        self.services = unsome(service)
        self.auto_updates = auto_updates
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return self._name

    def get_remote_version(self) -> str:
        if self.auto_updates:
            return "latest"
        return INSTALLER.latest_version(self.name)

    @property
    def local_version(self) -> Optional[str]:
        if self.auto_updates:
            return "latest" if INSTALLER.is_installed(self.name) else None
        return INSTALLER.installed_version(self.name)

    def postinstall_linux(self):
        run("sudo", "systemctl", "daemon-reload")
        for service in self.services:
            run("sudo", "systemctl", "enable", service)

    def postinstall_macos(self):
        pass

    def install(self):
        INSTALLER.install(self.name)
        with_os(linux=self.postinstall_linux, macos=self.postinstall_macos)()
