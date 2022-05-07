from dataclasses import dataclass

from .base import Storage


@dataclass
class Version:
    version: str


VERSIONS = Storage[Version]("version", Version)
