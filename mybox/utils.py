import concurrent.futures
import os
import subprocess
import sys
from os.path import dirname
from typing import Callable, Iterable, Literal, Optional, TypeVar, Union, cast

import tqdm  # type: ignore

OS = Literal["linux", "darwin"]

CURRENT_OS: OS = cast(OS, sys.platform)

T = TypeVar("T")
U = TypeVar("U")


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


def flatten(items: Iterable[Iterable[T]]) -> list[T]:
    return [item for sublist in items for item in sublist]


def parallel_map_tqdm(items: list[T], action: Callable[[T], U]) -> list[U]:
    with tqdm.tqdm(total=len(items)) as progress:
        with concurrent.futures.ThreadPoolExecutor(20) as executor:

            def action_and_update(item: T) -> U:
                result = action(item)
                progress.update(1)
                return result

            return list(executor.map(action_and_update, items))
