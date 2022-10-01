from pydantic import BaseSettings
from functools import lru_cache
from pydantic.networks import PostgresDsn


class DBSetting(BaseSettings):
    db_url:PostgresDsn
    ALGORITHM:str
    ACCESS_TOKEN_EXPIRE_MINUTES:int
    secret:str



@lru_cache
def get_settings():
    return DBSetting(_env_file='.env').dict()



