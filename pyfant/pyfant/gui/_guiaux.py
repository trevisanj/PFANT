__all__ = ["MONO_FONT"]

from PyQt4.QtGui import QFont

MONO_FONT = QFont("not_a_font_name")
MONO_FONT.setStyleHint(QFont.TypeWriter)
