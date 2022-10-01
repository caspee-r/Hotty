import os
import secrets
from datetime import datetime
from dateutil.relativedelta import relativedelta
from fastapi import (
    FastAPI,
    Query,
    Request,
    Response,
    HTTPException,
    status,
    Form,
    Path,
    Depends,
)
from fastapi.responses import RedirectResponse
# import imgkit
import schemas, models, database
from utils import hash_user_password, tokenize, ACCESS_TOKEN_EXPIRE_MINUTES
# routers
from routers import screenshots, users, auth
app = FastAPI(title="screenshot website")

# including routers
app.include_router(screenshots.screenshot)
# app.include_router(.screenshot)
app.include_router(auth.authentication)



# create the database metadata
models.base.metadata.create_all(database.engine)

# Global Vars
Cokie_duration_time = {
    "years": 0,
    "months": 6,
    "days": 0,
}  # y/m/d /h/m/s







