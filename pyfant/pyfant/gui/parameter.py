"""
Non-visual classes for defining parameters
"""

__all__ = ["Parameter", "Parameters"]

from PyQt4.QtGui import *

class Parameters(object):
    """
    Collection of parameters.

    Arguments:
      specs -- [(name, {...}), ...] see Parameter.FromSpec() for full documentation.
    """
    def __init__(self, specs):
        # List of Parameter objects
        self.params = []
        # Original specs (this never gets updated)
        self._specs = specs
        self._FromSpecs(specs)

    def _FromSpecs(self, specs):
        """
        Populates _params using a list of dictionaries.

        Arguments:
          specs -- List as [(name, {...}), ...].
               See Parameter.FromSpec() for documentation on specs
        """
        for spec in specs:
            self.params.append(Parameter(spec))

    def AddToLayout(self, layout):
        """
        Arguments:
          layout -- a QFormLayout instance
        """
        for param in self.params:
            widget = param.RenderWidget()
            layout.addRow(param.labelText, widget)

    def UpdateFromWidgets(self):
        for param in self.params:
            try:
                param.UpdateValueFromWidget()
            except Exception as e:
                raise
                # raise type(e)("Error in '%s': %s" % (param.name, +str(e)), None, sys.exc_info()[2]


    def GetKwargs(self):
        ret = {}
        for param in self.params:
            ret[param.name] = param.value
        return ret

    def GetSpecs(self):
        """Returns copy of self._specs with updated "values"."""
        ret = []
        for name, options in self._specs:
            ret.append((name, options.copy()))
        for param in self.params:
            for name, options in ret:
                if name == param.name:
                    options["value"] = param.value
                    break
        return ret


class Parameter(object):
    def __init__(self, spec=None):
        self.name = None
        self.labelText = None
        self.toolTip = None
        self.type = None
        self.value = None
        self.widget = None  # Widget that will edit the thing
        if spec is not None:
            self.FromSpec(spec)

    def FromSpec(self, spec):
        """
        Arguments:
          spec -- (name, {...})

        Dict keys:
          "labelText" -- (optional) text for label in editor. Defaults to the
                 keyword argument name
          "toolTip" (optional)
          "type" -- (optional, defaults to type("value") or int if "value" is
              not specified. Accepts:
                - int
                - float
                - str
                - bool
          "value" -- (optional) defaults to 1 if numeric, False if bool,
               "" if str

        Note that if type is not specified,
        """
        self.name, d = spec
        self.labelText = d.get("labelText", self.name)
        self.toolTip = d.get("toolTip", "")
        t = self.type = d.get("type", type(d["value"]) if "value" in d else int)
        assert t in (int, float, bool, str), "Invalid type: `%s`" % t.__name__
        self.value = d.get("value", 1 if t in (int, float) else False if t == bool else "")

    def RenderWidget(self):
        t = self.type
        if t == int:
            ret = QSpinBox()
            ret.setMaximum(999999999)
            ret.setValue(self.value)
        elif t == float:
            ret = QLineEdit()
            ret.setText(str(self.value))
        elif t == bool:
            ret = QCheckBox()
            ret.setChecked(self.value)
        else:  # only str left
            ret = QLineEdit()
            ret.setText(self.value)
        self.widget = ret
        return ret

    def UpdateValueFromWidget(self):
        t, w = self.type, self.widget
        if t == int:
            self.value = int(w.value())
        elif t == float:
            self.value = float(eval(str(w.text())))
        elif t == bool:
            self.value = w.isChecked()
        else:  # only str left
            self.value = str(w.text())
