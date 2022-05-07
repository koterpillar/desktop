import argparse
import os

import tqdm  # type: ignore
import yaml

from .package import Package, parse_package
from .utils import ROOT_DIR


def parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Install the environment")
    parser.add_argument("component", nargs="*", help="The component to install")
    return parser


def load_packages(component: str) -> list[Package]:
    with open(os.path.join(ROOT_DIR, "packages", f"{component}.yaml")) as f:
        packages = yaml.safe_load(f)
        return list(map(parse_package, packages))


def main():
    args = parser().parse_args()
    components: frozenset[str] = frozenset(args.component) | {"base"}
    packages = [
        package for component in components for package in load_packages(component)
    ]
    with tqdm.tqdm(total=len(packages)) as progress:
        for package in packages:
            progress.set_description(package.package_name())
            package.ensure()
            progress.update(1)


main()
