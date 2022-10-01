from enum import unique
from sqlalchemy import (
    Column,
    String,
    Integer,
    Boolean,
    ForeignKey,
    LargeBinary,
    DateTime,
)
from sqlalchemy.orm import declarative_base, relationship

base = declarative_base()


class User(base):
    __tablename__ = "users"
    id = Column(Integer, primary_key=True)
    username = Column(String(length=30), nullable=False, unique=True)
    email = Column(String, nullable=False, unique=True)
    password = Column(String, nullable=False)
    screenshots = relationship("Screenshot", back_populates="user")
    creation_date = Column(DateTime, nullable=False)


class Screenshot(base):
    __tablename__ = "screenshots"
    id = Column(Integer, primary_key=True)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    user = relationship("User", back_populates="screenshots")
    url = Column(String, nullable=False)
    image = Column(LargeBinary, nullable=False)
    creation_date = Column(DateTime, nullable=False)
