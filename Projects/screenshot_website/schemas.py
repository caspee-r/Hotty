from enum import Enum, auto
from typing import List
from datetime import datetime
from pydantic import BaseModel, EmailStr, HttpUrl
from pydantic.fields import Field


class SupportedImageType(Enum):
    png = "png"
    jpg = "jpg"


class NewScreenshot(BaseModel):
    url: HttpUrl
    name: str = Field(max_length=20, min_length=1)
    image_type: SupportedImageType = SupportedImageType.png


class ScreenshotIn(NewScreenshot):
    size: str
    created_at: datetime


class ScreenshotsOut(BaseModel):
    screenshots: List[ScreenshotIn]


class User(BaseModel):
    username: str = Field(max_length=30)


class UserLogin(User):
    password: str


class UserIn(UserLogin):
    email: EmailStr


class UserOut(User):
    email: EmailStr
    screenshot_numbers: int = Field(default=0)

    class config:
        orm_mode = True


class SecUser(User):
    password: str = Field(max_length=8)


class Token(BaseModel):
    access_token: str | None
    token_type: str = "Bearer"


class TokenData(BaseModel):
    username: str | int
