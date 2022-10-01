import sys
from PyQt5 import QtCore, QtGui
from PyQt5.QtWidgets import QApplication, QMainWindow, QLabel

class MyApp(QMainWindow):
    def __init__(self) -> None:
        super().__init__()
        self.label = QLabel(self)
        image = "/home/sammy/Pictures/Eyes.jpeg"
        with open(image)  :
            pixmap = QtGui.QPixmap(image)
            self.label.setPixmap(pixmap)
            self.label.move(25,40)


if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = MyApp()
    # w.resize(900,300)
    w.setWindowTitle("Sami")
    w.show()
    sys.exit(app.exec_())
