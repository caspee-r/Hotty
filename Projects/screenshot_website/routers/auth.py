import os
from datetime import datetime
from typing import Any
from fastapi import Depends, HTTPException, status, APIRouter
from jose import JWTError,jwt
from fastapi.security.oauth2 import OAuth2PasswordBearer, OAuth2PasswordRequestForm
from sqlalchemy.orm import Session
import database, schemas, models
from utils import jwt_decode, hash_user_password, tokenize, ACCESS_TOKEN_EXPIRE_MINUTES


authentication = APIRouter(tags=[ "Auth" ])

oauth2_scheme = OAuth2PasswordBearer(tokenUrl="login")



def verify_token(token: str, credentials_exception):

    try:
        payload = jwt_decode(token)
        username: Any | None = payload.get("sub")
        if username is None:
            raise credentials_exception
        token_data = schemas.TokenData(username=username)

    except JWTError:
        raise credentials_exception
    return token_data



def get_current_user(token: str = Depends(oauth2_scheme)):
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Could not validate credentials",
        headers={"WWW-Authenticate": "Bearer"},
    )
    return verify_token(token, credentials_exception)



@authentication.post("/register")
async def register_new_user(
        user: schemas.UserIn, session = Depends(database.get_db)
        ):

    user.password = hash_user_password(user.password)
    if not database.get_user(user.username,session):
        new_user = models.User(username=user.username,email=user.email,
                password=user.password,creation_date=datetime.now())
        
        session.add(new_user)
        session.commit()
        os.mkdir(f"../screenshots/{new_user}")
        return new_user
    return HTTPException(status.HTTP_403_FORBIDDEN,
            detail="The Username is already in use")

@authentication.post("/login", status_code=status.HTTP_200_OK,response_model=schemas.Token)
async def login(
    user:OAuth2PasswordRequestForm=Depends(),session=Depends(database.get_db),
):

    # check if the user in the database
    if database.check_user(user.username,user.password,session):
        # return Jwt
        token = tokenize({'sub':user.username},ACCESS_TOKEN_EXPIRE_MINUTES)
        return schemas.Token(access_token=token)
    else:
        return HTTPException(status.HTTP_401_UNAUTHORIZED,
                detail="invalid credential",
                headers={"WWW-Authenticate": "Bearer"},
                )

