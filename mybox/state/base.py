import dataclasses
import os
import sqlite3
import threading
from functools import cached_property
from typing import Generic, Type, TypeVar

T = TypeVar("T")

from ..utils import ROOT_DIR

DB_PATH = os.path.join(ROOT_DIR, "state.sqlite")


DB_CACHE = threading.local()


def db() -> sqlite3.Connection:
    try:
        return DB_CACHE.connection
    except AttributeError:
        pass
    connection = sqlite3.connect(DB_PATH)
    connection.row_factory = sqlite3.Row
    DB_CACHE.connection = connection
    return connection


class Storage(Generic[T]):
    @cached_property
    def attributes(self) -> list[str]:
        return [field.name for field in dataclasses.fields(self.klass)]

    def __init__(self, name: str, klass: Type[T]) -> None:
        self.name = name
        self.klass = klass
        db().execute(
            f'CREATE TABLE IF NOT EXISTS {self.name} (id TEXT PRIMARY KEY, {", ".join(self.attributes)})'
        )

    def __getitem__(self, key: str) -> T:
        row = db().execute(f"SELECT * FROM {self.name} WHERE id = ?", (key,)).fetchone()
        if row:
            attributes = {key: row[key] for key in row.keys() if key != "id"}
            return self.klass(**attributes)
        raise KeyError(key)

    def __delitem__(self, key: str) -> None:
        db().execute(f"DELETE FROM {self.name} WHERE id = ?", (key,))

    def __setitem__(self, key: str, value: T) -> None:
        with db():
            del self[key]
            attr_clause = "?"
            attr_values = [key]
            for attribute in self.attributes:
                attr_clause += ", ?"
                attr_values.append(getattr(value, attribute))
            db().execute(f"INSERT INTO {self.name} VALUES ({attr_clause})", attr_values)
