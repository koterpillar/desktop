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

    def installed_version(self, package: str) -> Optional[str]:
        raise NotImplementedError()

    def is_installed(self, package: str) -> bool:
        return self.installed_version(package) is not None

    def latest_version(self, package: str) -> str:
        return "repository"


class Brew(Installer):
    def install(self, *packages: str) -> None:
        run("brew", "install", *packages)

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

    def is_installed(self, package: str) -> bool:
        return run_ok("rpm", "-q", "--whatprovides", package)


class Apt(Installer):
    def install(self, *packages: str) -> None:
        run("sudo", "apt", "install", "--yes", *packages)

    def is_installed(self, package: str) -> bool:
        return run_ok("dpkg", "-s", package)


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

    def __init__(self, *, name: str, service: Some[str] = None, **kwargs) -> None:
        self._name = name
        self.services = unsome(service)
        super().__init__(**kwargs)

    @property
    def name(self) -> str:
        return self._name

    def get_remote_version(self) -> str:
        return INSTALLER.latest_version(self.name)

    @property
    def local_version(self) -> Optional[str]:
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
