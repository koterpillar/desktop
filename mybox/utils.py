import os
import subprocess
import sys
from os.path import dirname
from pathlib import Path
from typing import Callable, Iterator, Literal, Optional, TypeVar, Union, cast

OS = Literal["linux", "darwin"]

CURRENT_OS: OS = cast(OS, sys.platform)

T = TypeVar("T")


def with_os(*, linux: T, macos: T) -> T:
    if CURRENT_OS == "linux":
        return linux
    elif CURRENT_OS == "darwin":
        return macos
    else:
        raise ValueError(f"Unexpected OS {CURRENT_OS}.")


Some = Optional[Union[T, list[T]]]


def unsome_(x: Some[T]) -> Optional[list[T]]:
    if x is None:
        return None
    if isinstance(x, list):
        return x
    return [x]


def unsome(x: Some[T]) -> list[T]:
    return unsome_(x) or []


def run(*args: str, **kwargs) -> subprocess.CompletedProcess:
    return subprocess.run(args, check=True, **kwargs)


def run_ok(*args: str, **kwargs) -> bool:
    return (
        subprocess.run(
            args,
            check=False,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            **kwargs,
        ).returncode
        == 0
    )


def run_output(*args: str, **kwargs) -> str:
    return (
        subprocess.run(args, stdout=subprocess.PIPE, **kwargs).stdout.decode().strip()
    )


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
    else:
        if sudo:
            run("sudo", "ln", "-s", "-f", "-T", source, target)
        else:
            os.symlink(source, target)