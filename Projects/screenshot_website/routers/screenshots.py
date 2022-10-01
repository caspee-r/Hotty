import os
from fastapi import APIRouter, Path, Query, Depends, status, HTTPException
import schemas, models, database 
import imgkit
from . import auth


screenshot = APIRouter(
    prefix="/screenshot",
    tags=["screenshot"],
    # dependencies=,
    # responses=
)


@screenshot.get("/", response_model=schemas.ScreenshotsOut)
async def screenshots(
    name: str = Query(default=None), user=Depends(auth.get_current_user)
):
    screens:List = []
    for img in os.listdirs(f"../screenshots/{user.username}"):
        ...       


@screenshot.get("/{id}")
async def get_screenshot(
    id: int = Path(default=1),
):
    ...


@screenshot.post("/",status_code=201)# response_model=schemas.ScreenshotIn)
async def new_screenshot(screenshot: schemas.NewScreenshot,
        user=Depends(auth.get_current_user)):
    output_path=f"./screenshots/{user.username}/{screenshot.name}.{screenshot.image_type.value}"
    if not os.path.isfile(output_path):
        imgkit.from_url(screenshot.url,output_path)
        return {"success"}
    return HTTPException(400,detail="File Already exist , delete it then resubmit")



