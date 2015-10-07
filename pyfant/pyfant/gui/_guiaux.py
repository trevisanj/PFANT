__all__ = ["MONO_FONT", "SOL_HEADERS", "SOL_ATTR_NAMES"]

from PyQt4.QtGui import QFont

MONO_FONT = QFont("not_a_font_name")
MONO_FONT.setStyleHint(QFont.TypeWriter)


# Relating tablewidget columns with set-of-lines attributes
SOL_HEADERS = ["lambda", "sj", "jj"]
SOL_ATTR_NAMES = ["lmbdam", "sj", "jj"]
