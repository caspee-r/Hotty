from fastapi import APIRouter, Path
from fastapi.responses import JSONResponse
import schemas, database, models


users = APIRouter(
       prefix="/users",
        tags=['users'],)

@users.get("/", response_model=schemas.UserOut)
async def new_user(user:schemas.UserIn, ):
    ...

# @users.post("")
