
#include <Camera.h> //カメラを使う
#include <SDHCI.h>  //SDカードを使う
#include <stdio.h>  //sprintf に必要

SDClass  theSD; // SDカードは自分で宣言する。　一方、theCameraは定義されているので不要

//PC側シリアルボーレートと同じ値に
#define BAUDRATE       2000000

//Variables
const int MAX_FRAME_COUNT= 100;
int  frameNo=0;
char transferCmd = 0;
char preTransferCmd=0;
//bool callBackProcess = false;
bool callbackEndFlg = false;
bool shotFlg = false;
int take_picture_count = 0;

//Functions

void Debug(String s){
  // DebugとしてPC側に送れる文字は ASCII文字、かつ、64文字以内とする （64文字以上の場合、PC側はシリアルバッファ異常と判断しPurgeする）
  Serial.print("@"+s+"\n");
  Serial.flush();
}

void responseMsg(String s){
  Serial.print(s+"\n");
  Serial.flush();
}

void transferBinary(char* pData,int nDataSize){
  Serial.write(pData,nDataSize);
  Serial.flush();
}

bool checkCommand_InCallback() {
  char cmd;

  if (Serial.available() >0){
    cmd = Serial.read();
    if (cmd == 'E'){
      transferCmd = 0;
      //theCamera.startStreaming(false, CamCB);  Not Good in Camera Streaming Callback
      responseMsg("OK");
      callbackEndFlg = true;
      return true;
    }
    if (cmd=='S'){
      preTransferCmd= transferCmd;
      transferCmd =0;
      shotFlg = true;
      return true;
    }
  }
  return false;
}


void CamCB(CamImage img) {

  if (img.isAvailable() == false) return;
  if (transferCmd == 0) return;
  //if (callBackProcess == true) return;
  if (checkCommand_InCallback()== true) return;

  //callBackProcess = true;

  digitalWrite(LED0, HIGH);
  switch(transferCmd){
    case 'G':
      img.convertPixFormat(CAM_IMAGE_PIX_FMT_GRAY);
      break;
    case 'R':
      img.convertPixFormat(CAM_IMAGE_PIX_FMT_RGB565);
      break;
  }
  digitalWrite(LED0, LOW);
   
  char *buf = img.getImgBuff();
  int bufSize= img.getImgSize();

  // Imageデータの送信(Binary)
  Debug("CamCB");
  responseMsg("Image");
  frameNo++;
  String s= "FrameNo=" + String(frameNo);
  responseMsg(s);
  responseMsg("Type=" + String(transferCmd));
  s= "ImageSize="+ String(bufSize);
  responseMsg(s);
  digitalWrite(LED1, HIGH);
  transferBinary(buf,bufSize);
  digitalWrite(LED1, LOW);

  //callBackProcess = false;

  //沢山送ったらEを待たずに転送コマンド終了
  if (frameNo >= MAX_FRAME_COUNT)
    transferCmd = 0;
}

void checkCommand_InMainLoop()
{
  char cmd;
  if (Serial.available() >0)
  {
    cmd = Serial.read();
    Debug( "Spresense Recv="+String(cmd) );
    if ((cmd == 'G') || (cmd == 'R')) {
      theCamera.startStreaming(true, CamCB);
      theCamera.setAutoWhiteBalanceMode(CAM_WHITE_BALANCE_DAYLIGHT);
      frameNo = 0;
      transferCmd=cmd;
    }
    else if (cmd == 'S'){
      Debug("Shot Command");
      preTransferCmd= 0;
      transferCmd =0;
      shotFlg = true;     
    }
    else if (cmd == 'E'){
      responseMsg("OK");
    }
  }
}

void setup() {

  //Serialを利用可能に
  Serial.begin(BAUDRATE);
  while (!Serial) {};

  //カメラを利用可能に
  theCamera.begin();
/* FULLHDや5Mは img.Available で失敗する。
  theCamera.setStillPictureImageFormat(
     CAM_IMGSIZE_FULLHD_H,
     CAM_IMGSIZE_FULLHD_V,
     CAM_IMAGE_PIX_FMT_JPG);
*/

// QUADVGA = 1280x960  はOK
  theCamera.setStillPictureImageFormat(
     CAM_IMGSIZE_QUADVGA_H, //CAM_IMGSIZE_FULLHD_H,
     CAM_IMGSIZE_QUADVGA_V, //CAM_IMGSIZE_FULLHD_V,
     CAM_IMAGE_PIX_FMT_JPG);

  //SDカードを利用可能に
  while (!theSD.begin()) {
    ;
  }
  if (theSD.beginUsbMsc()!=0) {
    Debug("USBが準備されていません");
  }
}

void loop() {
  digitalWrite(LED3, HIGH);
  
  if (callbackEndFlg){
    theCamera.startStreaming(false,CamCB);
    callbackEndFlg = false;
  }
  if (shotFlg){
    shotFlg=false;
    Debug("Shot in MainLoop..　Stop Streaming");
    theCamera.startStreaming(false,CamCB);

    Debug("takePicture()");
    CamImage img = theCamera.takePicture();

    if (img.isAvailable()) {
      String s = "SHOT"+String(take_picture_count)+".JPG";
      Debug("Filename: "+s);

      theSD.endUsbMsc();
      //---------------------SDカードの操作をする間は UsbMscを停止する
      File myFile = theSD.open(s.c_str(), FILE_WRITE);
      myFile.write(img.getImgBuff(), img.getImgSize());
      myFile.close();
      //---------------------UsbMscを再開
      theSD.beginUsbMsc();

      take_picture_count++;
    }
    else
      Debug("img is not Available()");
      
    transferCmd = preTransferCmd;
    if (transferCmd!=0){
      Debug("Restart Streaming");
      theCamera.startStreaming(true, CamCB);
    }
  }
  
  if (transferCmd==0){
    checkCommand_InMainLoop();
  }
  digitalWrite(LED3, LOW); 
}
