from datetime import datetime, timedelta
from  passlib.context import CryptContext
from config import get_settings
from jose import JWTError, jwt

Algo= get_settings()['ALGORITHM']
ACCESS_TOKEN_EXPIRE_MINUTES = get_settings()['ACCESS_TOKEN_EXPIRE_MINUTES']
secret = get_settings()['secret']

contx = CryptContext(schemes=['bcrypt'])

def hash_user_password(password):
    return contx.hash(password)
def verify_password(password,hashed_password):
    return contx.verify(password,hashed_password)

def tokenize(data:dict, expires_delta: timedelta|None = None):
    to_encode = data.copy()
    if expires_delta:
        expire = datetime.utcnow() + timedelta(minutes=expires_delta)
    else:
        expire = datetime.utcnow() + timedelta(minutes=15)
    to_encode.update({"exp": expire})
    encoded_jwt = jwt.encode(to_encode, secret, algorithm=Algo)
    return encoded_jwt

def jwt_decode(token, secret=secret, algo=Algo):
    return jwt.decode(token,secret,algo)




