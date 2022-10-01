from sqlalchemy import create_engine
from sqlalchemy.orm import Session, sessionmaker
from config import get_settings
from utils import verify_password 
import models





engine = create_engine(url=get_settings()['db_url'])
LocalSession = sessionmaker(engine)

def get_db():
    db = LocalSession()
    try:
        yield db
    finally:
        db.close()

def add_user(username:str,email:str, password:str, session:Session):
    if not session.query(models.User).filter(models.User.username == username)\
            .first():
        user = models.User(username=username,password=password)
        session.add(user)
        session.commit()
        session.close()
        return True, user.id
    return False

def get_user(username:str, session:Session):
    if (user:=session.query(models.User).filter(models.User.username==username)\
            .first()):
        return user
    return False


def del_user():
    ...


def update_user():
    ...

def check_user(username,password,session:Session):
    if not (user:=session.query(models.User)\
            .filter(models.User.username == username).first()):
        return False
    
    if not verify_password(password,user.password):
        return False
    return user




def add_screenshot():
    ...


def del_screenshot():
    ...








