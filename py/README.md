# Virtual environments

```
mkdir my_project
cd my_project
python3 -m venv .venv
source .venv/bin/activate
```

Then, in VS Code's bottom left interpreter, select the correct python interpreter, `./.venv/bin/python` (the `.` indicates current working directory), which is in the virtual environment you just created.

Next, install packages you might need:

```
pip install package_1 ...
```

Type `deactive` to deactivate the virtual environment when done.

To reactivate, `source .venv/bin/activate`.

Add `tasks.json` in the working dir with the following to run any py file with `Cmd + Shift + B`: 

```
{
	"version": "2.0.0",
	"command": "python3",
	"reveal": "always",
	"args": [
		"${file}"
	]
}
```


# IPython in VS Code

Add a `# %%` to turn sections of a py program into an IPython notebook cell.
