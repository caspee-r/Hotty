import sys
import secrets
from PyQt6.QtWidgets import (
    QApplication,
    QCheckBox,
    QDialog,
    QLineEdit,
    QMainWindow,
    QLabel,
    QWidget,
    QPushButton,
    QMessageBox,
)
from PyQt6.QtCore import QRect, Qt
from PyQt6.QtGui import QPixmap, QFont


class MyWindow(QMainWindow):
    def __init__(self) -> None:
        super().__init__()
        LoginLabel = QLabel("Login", self)
        LoginLabel.move(130, 0)
        # username and password labels
        UsernameLabel = QLabel("Username", self)
        UsernameLabel.setFont(QFont("Fira Code", 11))
        UsernameLabel.move(20, 50)
        PasswordLabel = QLabel("Password", self)
        PasswordLabel.setFont(QFont("Fira Code", 11))
        PasswordLabel.move(20, 100)
        # QLineEdit
        UsernameLine = QLineEdit(self)
        UsernameLine.setGeometry(0,0,200,20)
        UsernameLine.move(100, 55)
        UsernameLine.setClearButtonEnabled(True)
        self.PasswordLine = QLineEdit(self)
        self.PasswordLine.setEchoMode(QLineEdit.EchoMode.Password)
        self.PasswordLine.setGeometry(0,0,200,20)
        self.PasswordLine.setClearButtonEnabled(True)
        self.PasswordLine.move(100, 105)
        ShowPasswordBox = QCheckBox(self)
        ShowPasswordBox.clicked.connect(self.show_password)
        ShowPasswordBox.move(70, 129)
        ShowPasswordLabel = QLabel("Show Password", self)
        ShowPasswordLabel.setFont(QFont("Fira Code", 8))
        ShowPasswordLabel.move(89, 129)
        LoginBtn = QPushButton("Login", self)
        LoginBtn.setGeometry(0,0,300,25)
        LoginBtn.move(20, 170)
        NotMemLabel = QLabel("Not a Member?", self)
        NotMemLabel.setGeometry(0,0,110,25)
        NotMemLabel.move(50, 200)
        NotMemLabel.setFont(QFont("Fira Code", 10))
        signupbtn = QPushButton("Sign Up", self)
        signupbtn.setGeometry(0,0,55,25)
        signupbtn.move(165, 200)
        signupbtn.setStyleSheet("border-radius:10px;background-color:#983323;selection-background-color:#ffffff")

        signupbtn.clicked.connect(self.signup)
        self.initializeUi()

    def signup(self):
        self.SignupDialog = SignUp()
        self.SignupDialog.show()

    def show_password(self):
        if self.PasswordLine.echoMode() ==QLineEdit.EchoMode.Password:
            self.PasswordLine.setEchoMode(QLineEdit.EchoMode.Normal)
        else:
            self.PasswordLine.setEchoMode(QLineEdit.EchoMode.Password)
    def closeEvent(self,event) -> None:
        answer = QMessageBox.question( self, "Quit Application?", "Are you sure you want to QUIT?",
                QMessageBox.StandardButton.No | \
                        QMessageBox.StandardButton.Yes,
                        QMessageBox.StandardButton.Yes)
        if answer == QMessageBox.StandardButton.Yes:
            event.accept()
        if answer == QMessageBox.StandardButton.No:
            event.ignore()



    def initializeUi(self):
        self.setGeometry(100, 100, 350, 400)
        self.setWindowTitle("Login")
        self.setFont(QFont("Fira Code", 14))
        self.show()


class SignUp(QDialog):
    def __init__(self) -> None:
        super().__init__()
        self.NewAccountLabel = QLabel("Create New Account", self)
        self.NewAccountLabel.setFont(QFont("Fira Code",14))
        self.NewAccountLabel.move(70, 20)
        __image = r"/home/sammy/Pictures/Eyes.jpeg"
        self.pixmap = QPixmap(__image)
        self.Avatar = QLabel(self)
        self.Avatar.setPixmap(self.pixmap)
        self.Avatar.setGeometry(30,45,250,120)
        self.UsernameLabel = QLabel("Username:", self)
        self.UsernameLabel.move(20,180)
        self.UsernameLine = QLineEdit(self)
        self.UsernameLine.setGeometry(0,0,200,20)
        self.UsernameLine.move(80,180)
        self.FullNameLabel = QLabel("Full Name:", self)
        self.FullNameline = QLineEdit(self)
        self.FullNameline.setGeometry(0,0,200,20)
        self.FullNameline.move(80,210)
        self.FullNameLabel.move(20,210)
        self.PasswordLabel = QLabel("Password:", self)
        self.PasswordLine = QLineEdit(self)
        self.PasswordLine.setGeometry(0,0,200,20)
        self.PasswordLine.setEchoMode(QLineEdit.EchoMode.Password)
        self.PasswordLine.move(80,240)
        self.PasswordLabel.move(20,240)
        self.ConfirmLabel = QLabel("Confirm:", self)
        self.ConfirmLine = QLineEdit(self)
        self.ConfirmLine.setEchoMode(QLineEdit.EchoMode.Password)
        
        self.ConfirmLine.setGeometry(0,0,200,20)
        
        self.ConfirmLine.move(80,270)
        self.ConfirmLabel.move(20,270)
        self.SignUpBtn = QPushButton("Sign Up",self)
        self.SignUpBtn.setStyleSheet("background-color:#37ff6f;border-radius:10px;color:white")
        self.SignUpBtn.setFont(QFont("Fira Code",12))
        self.SignUpBtn.setGeometry(0,0,250,25)
        self.SignUpBtn.move(20,300)
        self.initializeUi()

    def initializeUi(self):
        self.setGeometry(120,100,300,400)
        self.setWindowTitle("SignUp")
        self.setFixedSize(300,400)



if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = MyWindow()

    sys.exit(app.exec())
