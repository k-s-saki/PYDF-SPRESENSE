
#include <Camera.h> // カメラを使う
#include <stdio.h>  // sprintf に必要

#ifdef _DNNRT_

#include "ge.h"     // ラスタオペレーション(SONYさまソース改造)
#include <DNNRT.h>
DNNRT dnnrt;      // DNNRTオブジェクト

#include <SDHCI.h>  //SDカードを使う--＞使いません
SDClass  theSD; // SDカードは自分で宣言する。　一方、theCameraは定義されているので不要

#endif

//Typedef.---------------------------------------------------

/* USB通信 */
//PC側シリアルボーレートと同じ値に
#define BAUDRATE       2000000
const String EOL="\n";

/* カメラモード */
enum CameraMode {
  cmNone,
  cmCBPreview,           // Camera Callback (YUV)
  cmJpegPreview,         // Jpeg Stream  
  cmJpegFile,            // Shot JpegFile
  cmPgmFile              // Shot PgmFile 
};
const int MAX_FRAME_COUNT= 1000;

/* 表示LED */
const int LED_SERIAL=LED0;  //シリアルが機能中
const int LED_CAMERA=LED1;  //カメラが機能中
const int LED_DNN   =LED2;  //DNN機能中
const int LED_SD    =LED3;  //SD機能中
const int LED_LOOP  =LED3;  //メインのループ

// Local Variables----------------------------------------------------

/* カウンタ */
int jpegFileNo = 0;
int pgmFileNo = 0;
int  frameNo=0;

/* フラグ */
bool isTransfer = false;
bool beingCaptured = false;

/* カメラモード */
CameraMode cameraMode = cmNone;

//For Debug
int  stopCounter=0;
int  loopCounter=0;

// Local Functions ------------------------------------------------------

void ErrorStop(String s){
  Debug("STOP:"+s);
  stopCounter = 100;
}

void Debug(String s){
  digitalWrite(LED_SERIAL, HIGH);
  // DebugとしてPC側に送れる文字は ASCII文字、かつ、64文字以内とする （64文字以上の場合、PC側はシリアルバッファ異常と判断しPurgeする）
  s=String("@")+s+String(EOL);
  Serial.print(s);
  Serial.flush();
  digitalWrite(LED_SERIAL, LOW);
}

void responseMsg(String s){
  digitalWrite(LED_SERIAL, HIGH);
  s=s+String(EOL);
  Serial.print(s);
  Serial.flush();
  digitalWrite(LED_SERIAL, LOW);
}

void transferBinary(uint8_t* pData,int nDataSize){
  digitalWrite(LED_SERIAL, HIGH);
  Serial.write(pData,nDataSize);
  Serial.flush();
  digitalWrite(LED_SERIAL, LOW);
}


bool StopCapture(){
  bool bRet=false;
  
  if (beingCaptured){
    Debug("Stop caputure");
    if (theCamera.capturePictureStop()==false){
      ErrorStop("Stop Capture fail.");
    }
    else{
      Debug("Stop caputure success");
      bRet=true;
    }
    beingCaptured = false;
  }
  else{
    Debug("Capture Already Stoped. ");    
  }

  return bRet;
}

bool StartCapture(){
  if (!beingCaptured){
    Debug("Start caputure");
    beingCaptured = theCamera.capturePictureStart();
    if (beingCaptured==false){
      ErrorStop("Start Capture fail.");
    }
    else
      Debug("Start caputure success");
  }
  else{
    Debug("Error: Capture Already Started ");
  }
  return beingCaptured;
}


char checkSerialCommand()
{
  /* コマンドは1文字で判定 */
  char cmd;
  if (Serial.available() >0)
  {
    cmd = Serial.read();
    Debug( "Spresense Serial Recv="+String(cmd) );
    if (cmd == 'C') {
      if (!isTransfer){
        Debug("Capture Command");
        CmdCaptureStart();
      }
      else
        Debug("Already Start Capture");
    }
    else if (cmd == 'S'){
      Debug("Shot Command");
      if (beingCaptured)
        CmdCaptureEnd();

      CmdShot();
    }
    else if (cmd == 'P'){
      Debug("PGM Command");
      if (beingCaptured)
        CmdCaptureEnd();

      CmdGrayPgm();
    }
    else if (cmd == 'E'){
      Debug("EndCapture Command");
      CmdCaptureEnd();
      responseMsg("OK");
    }
    else if (cmd=='D'){
      Debug("DNN Start Command");
      if (beingCaptured){
        //CmdCaptureEnd();
      }
      
      CmdDNN(true);
      responseMsg("OK");
    }
    else if (cmd=='d'){
      Debug("DNN End Command");
      if (beingCaptured){
        //CmdCaptureEnd();
      }
      
      CmdDNN(false);
      responseMsg("OK");
    }
    else{
      Debug( "Undefined Command :"+String(cmd));
    }

    return cmd;
  }
  return 0;
}

#ifdef _DNNRT_
uint8_t img_mem[28 * 28] __attribute__((aligned(16)));
DNNVariable dnn_in(28 * 28);
bool findPiyo = false;
#endif

#define WHITE_BALANCE_FRAMES 24
int nframes;

void CamCB(CamImage img)
{

  if (img.isAvailable())
  {

    // Check frame counts for stop auto white balance
    if (nframes == WHITE_BALANCE_FRAMES)
    {
      // Disable auto white balance

      theCamera.setAutoWhiteBalance(false);
    }
    nframes++;

    //とりあえず4フレームおきに処理
    if (nframes % 4 !=0)
      return;
    
#ifdef _DNNRT_
    float * pInputData;
    unsigned int i;

    // ge.h / cpp はSONYさん製作のラスターオペレーション（画像の便利関数）関数がはいってました。（ジャンケンのサンプルから取得して一部改変）
    // 決め打ちで228, 228 YUV から 28x28のメモリを作っているようですが、ビット操作ではなく、ハードウエアでやっている（ような）
    GE.shrink(img, img_mem);

    // 入力変数　dnn_in は1次元：要素数28x28で初期化されています。そこに画素データをセットします。
    pInputData = dnn_in.data();
    for (i = 0; i < 28 * 28; i++)
    {
      pInputData[i] = (float)img_mem[i];
    }

    // DNNRTにセットして・・
    dnnrt.inputVariable(dnn_in, 0);
    // 推論して
    dnnrt.forward();
    // 結果の取得
    DNNVariable dnn_out = dnnrt.outputVariable(0);

    char s[40];
    double oPiyo = dnn_out[1];
    //sprintf(s, "DNN Output [0]=%.4f  [1]=%.4f", dnn_out[0],dnn_out[1]);     
    if (oPiyo == 1.0)
      oPiyo = 0.0;

    sprintf(s, "Piyo %.8f ", oPiyo);
    String str(s);
    Debug(str);

    //Piyoかどうかの判定
    bool b = (oPiyo > 0.80);
    if (b && !findPiyo){
      Debug("Find Piyo!");
    }
    if (!b && findPiyo){
      Debug("Lost sight of Piyo!");
    }
    findPiyo = b;
#endif

  }
}
void SetCameraMode(CameraMode cm)
{
  if (beingCaptured){
    Debug("SetCamera Call Sequence Error: beingCaptured! ");
    return;
  }
  
  Debug("SetCamera theCamera.end() ");
  
  //現時点では、JPG->YUV422にするとき end() が必要になるようだ（少し時間がかかる）
  theCamera.end();
  theCamera.begin();
  Debug("SetCamera theCamera.begin() ");
  theCamera.setAutoWhiteBalanceMode(CAM_WHITE_BALANCE_DAYLIGHT);

  if (cm==cmCBPreview){
    Debug("Preview:CameraCallback");
    //カメラコールバックではフォーマットがYUVに固定されるので何もしない
    cameraMode= cm;   
  }
  if (cm==cmJpegPreview){
    Debug("Preview:QVGA/JPEG");
    theCamera.setStillPictureImageFormat(
       CAM_IMGSIZE_QVGA_H,
       CAM_IMGSIZE_QVGA_V,
       CAM_IMAGE_PIX_FMT_JPG);
    cameraMode= cm;
  }
  else if (cm==cmJpegFile){
    Debug("Shot:QVGA/JPEG");

    //メモリを沢山使っているためか、QUADVGAではエラーになるケースがある。QVGAではエラーにならない。
    theCamera.setStillPictureImageFormat(
//       CAM_IMGSIZE_QUADVGA_H,
//       CAM_IMGSIZE_QUADVGA_V,
       CAM_IMGSIZE_QVGA_H,
       CAM_IMGSIZE_QVGA_V,
       CAM_IMAGE_PIX_FMT_JPG);
    cameraMode= cm;
  }
  else if (cm==cmPgmFile){
    Debug("PGM:QVGA --> YUV422 ");
    theCamera.setStillPictureImageFormat(
       CAM_IMGSIZE_QVGA_H,
       CAM_IMGSIZE_QVGA_V,
       CAM_IMAGE_PIX_FMT_YUV422);
    cameraMode= cm;
  }
}

void CmdCaptureStart(){
  SetCameraMode(cmJpegPreview);
  frameNo = 0;
  isTransfer= true;
  Debug("CmdCaptureStart() StartCapture()");
  if (StartCapture()==false)
  {
    Debug("StartCapture fail");
  }
}

void CmdCaptureEnd(){
  isTransfer =false;
  if (StopCapture()==false)
  {
    Debug("StopCapture fail");
  }
}

void SendImageProperty(String kind, int frameNo, String imageType, int imageSize)
{
  String s;
  // Imageデータのプロパティ送信
  responseMsg(kind); 
  s= "FrameNo=" + String(frameNo);
  responseMsg(s);
  responseMsg("Type="+imageType);
  s= "ImageSize="+ String(imageSize);
  responseMsg(s); 
}

void CmdShot()
{
  Debug("CmdShot()");
  SetCameraMode(cmJpegFile);
  if (StartCapture()== false){
    Debug("CmdShot() CaptureStart Error ");
    return;
  }
  CamImage img = theCamera.capturePicture();
  if (img.isAvailable()) 
  {
      uint8_t* buf = img.getImgBuff();
      int bufSize = img.getImgSize();
      SendImageProperty("ImageFile", jpegFileNo, "JPEG", bufSize);    
      transferBinary(buf,bufSize);
      Debug("JPEG Picture File transfer end.");   
      jpegFileNo++;
/*
    String s = "SHOT"+String(jpegFileNo)+".JPG";
    Debug("Filename: "+s);
    theSD.endUsbMsc();
    //---------------------SDカードの操作をする間は UsbMscを停止する
    File myFile = theSD.open(s.c_str(), FILE_WRITE);
    myFile.write(img.getImgBuff(), img.getImgSize());
    myFile.close();
    //---------------------UsbMscを再開
    theSD.beginUsbMsc();
*/
  }
  else
    Debug("CmdShot(): img isAvailable()==false ");  

  img = theCamera.capturePicture();  // 現時点ではStopのために2回必要（captureが１回だとStopに失敗する)
  Debug("CmdShot(): Try Stop Capture. ");  
  if (StopCapture())
    Debug("CmdShot(): Try Stop Capture Success. ");
  else
    Debug("CmdShot(): Try Stop Capture Error. ");
}

void CmdGrayPgm(){
  Debug("CmdGrayPgm()");

  SetCameraMode(cmPgmFile);
  if (StartCapture()== false){
    Debug("CmdGrayPgm() CaptureStart Error ");
    return;
  }
  
  CamImage img = theCamera.capturePicture();
  if (img.isAvailable()) {
    CamImage clip_image;
    //(320x240) -Clip-> (224x224)-Resize-> (28x28)  // ByHW(=By Hardware accelarated function) 
    CamErr err = img.clipAndResizeImageByHW(clip_image,48,8,271,231,28,28);
    if (err) {
      Debug("Error : clipAndResizeImageByHW().");
    }
    else
    {
      if (clip_image.isAvailable())
      {
        String header="P5"+EOL+"28 28"+EOL+"255"+EOL;
        int bufSize = 28*28+header.length();
        SendImageProperty("ImageFile", pgmFileNo, "PGM", bufSize);
        pgmFileNo++;

        Serial.write(header.c_str());
        uint16_t* buf = (uint16_t*)clip_image.getImgBuff();

        //輝度情報のみを8bit値で記録
        for(int x=0; x<28; ++x){
          for(int y=0; y<28; ++y){
            uint8_t val=(uint8_t)(( *buf & 0xff00) >> 8);
            Serial.write(val);
            ++buf;
          }
        }
        Debug("Image File transfer end.");
      }
      else{
        Debug("clipAnd.. clip_image.isAvailable()==false ");
      }
    }
  }
  else
    Debug("CmdGrayPgm(): img isAvailable()==false ");  

  img = theCamera.capturePicture();  // 現時点ではStopのために2回必要（captureが１回だとStopに失敗する)
  Debug("CmdGrayPgm(): Try Stop Capture. ");  
  if (StopCapture())
    Debug("CmdGrayPgm(): Try Stop Capture Success. ");
  else
    Debug("CmdGrayPgm(): Try Stop Capture Error. ");
}

void streamImage(){
  if (!beingCaptured) 
  {
    ErrorStop("streamImage() Error: beingCaptured==false)");
    return;
  }
 
  CamImage img = theCamera.capturePicture();
  if (!img.isAvailable()) {
    ErrorStop("caputure img is not Available()");  
    return;
  }
  
  uint8_t* buf = img.getImgBuff();
  int bufSize= img.getImgSize();

  SendImageProperty("ImageStream", frameNo, "JPEG", bufSize);    
  transferBinary(buf,bufSize);
  // Debug("JPEG Picture Stream end.");
  frameNo++;
  
  //沢山送ったらEを待たずに転送コマンド終了
  if (frameNo >= MAX_FRAME_COUNT)
    isTransfer = false;
}

void Cmd(char cmd){
  if (cmd == 'S'){
    Debug("Cmd() Shot Command");
    CmdShot();
  }
  else if (cmd == 'P'){
    Debug("Cmd() PGM Command");
    CmdGrayPgm();
  }
  else{
    Debug("Cmd ???");
  }
}
bool isErrorStopInLoop()
{
  if(stopCounter>0){
    if (stopCounter % 10 ==0){
      Debug("stopCounter="+String(stopCounter));
    }
    delay(100);
    stopCounter--;
    return true;
  }
  return false;
}

void CmdDNN(bool bEnable){
  if (bEnable){
    Debug("START CameraCallback for DNN");
    theCamera.startStreaming(true, CamCB);
  }
  else{
    Debug("STOP CameraCallback for DNN");
    theCamera.startStreaming(false, CamCB);   
  }
}

//Override Functions of Arduino------------------------------------------------------

void setup() 
{
  digitalWrite(LED_SERIAL, HIGH);
  
  //Serialを利用可能に
  Serial.begin(BAUDRATE);
  while (!Serial) {};

  digitalWrite(LED_CAMERA, HIGH);

  //カメラを利用可能に
  theCamera.begin();
  theCamera.setAutoWhiteBalanceMode(CAM_WHITE_BALANCE_DAYLIGHT);

#ifdef _DNNRT_

  /* DNNRTを初期化 */
  
  digitalWrite(LED_DNN, HIGH);
  Debug("Loading DNN file");

  File nnbfile("Piyo_LeNet.nnb");
  if (!nnbfile) {
    Debug("DNN File Not found.");
    return;
  }

  Debug("Initialize DNNRT");
  int ret = dnnrt.begin(nnbfile);
  if (ret < 0){
    Debug("DNNRT initialize error.");
  }
#endif


  // 全LED点灯
  digitalWrite(LED0, HIGH);
  digitalWrite(LED1, HIGH);
  digitalWrite(LED2, HIGH);
  digitalWrite(LED3, HIGH);

  // 1sec 全LED点灯で準備完了をお知らせ
  delay(1000);  
  
  // 全LED消灯
  digitalWrite(LED0, LOW);
  digitalWrite(LED1, LOW);
  digitalWrite(LED2, LOW);
  digitalWrite(LED3, LOW);
}

void loop() {

  if (isErrorStopInLoop())
    return;

  loopCounter++;
  if (loopCounter / 1000 <= 1){  
    digitalWrite(LED_LOOP, HIGH);
  }
  if (loopCounter > 10000){
    loopCounter=0;
  }  
  if (checkSerialCommand()==0)
  {
    if ( isTransfer )
    {
      if (!beingCaptured)
      {
        if (StartCapture()==false)
        {
          Debug("loop() StartCapture()==false");
          stopCounter=100;       
        }
      }

      if(beingCaptured)
        streamImage();
    }  
  }
  digitalWrite(LED_LOOP, LOW); 
}
