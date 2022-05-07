import shutil
from abc import ABCMeta, abstractmethod

from ..utils import *
from .base import Package


class Installer(metaclass=ABCMeta):
    @abstractmethod
    def install(self, *packages: str) -> None:
        pass

    @abstractmethod
    def is_installed(self, package: str) -> bool:
        pass


class Brew(Installer):
    def install(self, *packages: str) -> None:
        run("brew", "install", *packages)

    def is_installed(self, package: str) -> bool:
        raise NotImplementedError()


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
        return "repository"

    @property
    def local_version(self) -> Optional[str]:
        if INSTALLER.is_installed(self.name):
            return "repository"
        else:
            return None

    def install(self):
        INSTALLER.install(self.name)
        run("sudo", "systemctl", "daemon-reload")
        for service in self.services:
            run("sudo", "systemctl", "enable", service)
