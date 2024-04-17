## Mock
Support `json` and `py` file type type of mocking.

## Developing

## Setup

First, install this package so that all modules can be imported by each other.

```py
python install -e .
```

### Add a module

When adding a new module, you must include it in the `py-module` of the `pyproject.toml` file.

```toml
py-modules = [
    "new_module"
]

```

### Import a module

Because this package is `flat-layout`, all modules should be imported directly without the package name.

E.g.:

```py
import new_module
```

## Testing

> [!NOTE]
> Notice that you need to finish the setup before running tests.

Run the tests.

```py
python -m pytest -s
```
