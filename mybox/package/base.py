from abc import ABCMeta, abstractmethod
from functools import cached_property
from typing import Optional

from ..utils import *


class Package(metaclass=ABCMeta):
    applicable: Optional[list[OS]]

    def __init__(self, applicable: Some[OS] = None) -> None:
        self.applicable = unsome(applicable)

    @abstractmethod
    def package_name(self) -> str:
        pass

    @abstractmethod
    def get_remote_version(self) -> str:
        pass

    @cached_property
    def remote_version(self) -> str:
        return self.get_remote_version()

    @abstractmethod
    def local_version(self) -> Optional[str]:
        pass

    def is_installed(self) -> bool:
        return self.remote_version == self.local_version()

    @abstractmethod
    def install(self) -> None:
        pass

    def ensure(self):
        if self.applicable is not None and CURRENT_OS not in self.applicable:
            return
        if self.is_installed():
            return
        self.install()
