import os
from os.path import dirname
from pathlib import Path
from typing import Iterator, Literal, Optional

from .utils import run

ROOT_DIR = dirname(dirname(__file__))


def home(*path: str) -> str:
    return os.path.join(os.environ["HOME"], *path)


def local(*path: str) -> str:
    return home(".local", *path)


def files_in_recursively(directory: str, glob: str = "*") -> Iterator[str]:
    for path in Path(directory).rglob(glob):
        yield str(path)


def transplant_path(dir_from: str, dir_to: str, path: str) -> str:
    path = os.path.relpath(path, dir_from)
    return os.path.join(dir_to, path)


def makedirs(path: str, sudo: bool = False) -> None:
    if sudo:
        run("sudo", "mkdir", "-p", path)
    else:
        os.makedirs(path, exist_ok=True)


def rm(path: str, sudo: bool = False) -> None:
    if sudo:
        run("sudo", "rm", "-r", "-f", path)
    else:
        if os.path.lexists(path):
            os.unlink(path)


def make_executable(path: str) -> None:
    run("chmod", "+x", path)


LinkMethod = Literal["binary_wrapper"]


def link(
    source: str, target: str, *, sudo: bool = False, method: Optional[LinkMethod] = None
) -> None:
    target_dir = os.path.dirname(target)
    makedirs(target_dir, sudo=sudo)
    rm(target, sudo=sudo)
    if method == "binary_wrapper":
        if sudo:
            raise NotImplementedError()
        else:
            with open(target, "w") as wrapper_file:
                print(f'#!/bin/sh\nexec "{source}" "$@"', file=wrapper_file)
            make_executable(target)
    else:
        if sudo:
            run("sudo", "ln", "-s", "-f", "-T", source, target)
        else:
            os.symlink(source, target)
