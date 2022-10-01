import sys
from PyQt6.QtWidgets import (
    QLabel,
    QButtonGroup,
    QCheckBox,
    QPushButton,
    QVBoxLayout,
    QWidget,
    QApplication,
)
from PyQt6.QtGui import QFont
from PyQt6.QtCore import Qt

class MainWindow(QWidget):
    def __init__(self):
        super().__init__()
        heading = QLabel("Chez Djellabi")
        heading.setFont(QFont("Fira Code",13))
        heading.setAlignment(Qt.AlignmentFlag.AlignCenter)
        qst = QLabel("How would you rate our services?")
        qst.setAlignment(Qt.AlignmentFlag.AlignTop)
        qst.setFont(QFont("Fira Code",10))
        lyt = QVBoxLayout()
        btn_group = QButtonGroup(self)
        satisfied = QCheckBox("Satisfied")
        average = QCheckBox("Average")
        not_satisfied = QCheckBox("Not Satisfied")
        self.confirm_btn = QPushButton("Confirm")
        self.confirm_btn.setEnabled(False)
        btn_group.addButton(satisfied)
        btn_group.addButton(average)
        btn_group.addButton(not_satisfied)
        btn_group.buttonClicked.connect(self.check_clicked)
        lyt.addWidget(heading)
        lyt.addWidget(qst)
        lyt.addWidget(satisfied)
        lyt.addWidget(average)
        lyt.addWidget(not_satisfied)
        lyt.addWidget(self.confirm_btn)

        self.setLayout(lyt)

        self.SetUp()

    def check_clicked(self):
        self.confirm_btn.setEnabled(True)

    def SetUp(self):
        self.setFixedSize(285,300)
        self.setWindowTitle("2022 Survey")
        self.show()



if __name__ == "__main__":
    app = QApplication(sys.argv)
    w = MainWindow()

    sys.exit(app.exec())

