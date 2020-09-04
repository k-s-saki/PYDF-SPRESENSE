
#include <Camera.h> // カメラを使う theCameraは定義されているので宣言不要
#include <stdio.h>  // sprintf に必要
#include <Arduino.h>
//#include <MP.h>  // マルチコア用

#include <DNNRT.h>
DNNRT dnnrt;      // DNNRTオブジェクト

#include <SDHCI.h>  //SDカードを使う
SDClass  SD; // SDカードのオブジェクト

//Typedef.---------------------------------------------------

/* USB通信 */
//PC側シリアルボーレートと同じ値に
#define BAUDRATE       2000000
const String EOL="\r\n"; //"\n";

/* カメラモード */
enum CameraMode {
  cmNone,
  cmCBPreview,        // Camera Callback (videoModeでフォーマットが決定される）
  cmJpegPreview,         // Jpeg Stream  
  cmJpegFile,            // Shot JpegFile
  cmPgmFile              // Shot PgmFile 
};
const int MAX_FRAME_COUNT= 1000;
const int MAX_READ_BYTES = 100; 
// 1KB(1024)だとシリアルは読める  // 2KBだと up_hardfault: PANIC!!! Hard fault: 40000000 エラーになる
// 1KBでSDに書こうとするとwrite()で0が帰ってくる

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
bool isDNNReady = false;


/* カメラモード */
CameraMode cameraMode = cmNone;

/* ビデオモード */
// Ver1.5から CAM_IMAGE_PIX_FMT_JPG がサポートされた。
CAM_IMAGE_PIX_FMT videoFormat= CAM_IMAGE_PIX_FMT_JPG;
//CAM_IMAGE_PIX_FMT videoFormat= CAM_IMAGE_PIX_FMT_YUV422;

/* Callbackに使われるモードと思われるが、YUV422しかテストしてない 
CAM_IMAGE_PIX_FMT_RGB565   RGB565 format
CAM_IMAGE_PIX_FMT_YUV422  YUV422 packed.
CAM_IMAGE_PIX_FMT_JPG   JPEG format
CAM_IMAGE_PIX_FMT_GRAY  Gray-scale
CAM_IMAGE_PIX_FMT_NONE  No defined format
*/

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
  // 仕様：DebugとしてPC側に送れる文字は ASCII文字、かつ、64文字以内とする （64文字以上の場合、PC側はシリアルバッファ異常と判断しPurgeする）
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

void sendBinaryData(uint8_t* pData,int nDataSize){
  digitalWrite(LED_SERIAL, HIGH);
  Serial.write(pData,nDataSize);
  Serial.flush();
  digitalWrite(LED_SERIAL, LOW);
}

/*
void recvBinaryDataToFile(File& aFile, int nDataSize){
  // 注：使っていない
  Debug("recvBinaryDataToFile datasize="+String(nDataSize));
  char buff[MAX_READ_BYTES+1];
  int readed=0;
  for(int readed=0; readed< nDataSize; ){
    
    int toReadBytes= min(MAX_READ_BYTES, nDataSize - readed );
    Debug("readed="+String(readed)+" toReadBytes="+String(toReadBytes) );

    int n = Serial.readBytes(buff,toReadBytes);
    Debug("serial readed="+String(n));
    if (n==0){
      Debug(s);
      Debug("zero bytes readed.");
      return;
    }
   
    readed= readed + n;
    int nWriteBytes = aFile.write(buff,n);
    Debug(" wrote bytes to file ="+String(nWriteBytes));
  }

  Debug("Success file Receive");
  Debug(s);  
}
*/

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
        CmdCaptureEnd();
      }
      
      CmdDNN(true);
      responseMsg("OK:DNN Start");
    }
    else if (cmd=='d'){
      Debug("DNN End Command");
      if (beingCaptured){
        CmdCaptureEnd();
      }
      
      CmdDNN(false);
      responseMsg("OK:DNN End");
    }
    else if (cmd=='U'){
      Debug("USBMSC Start Command");
      
      /* Initialize SD */
      while (!SD.begin()) {
        ; /* wait until SD card is mounted. */
      }
      
      /* Start USB MSC */
      if (SD.beginUsbMsc()) {
        Debug("USB MSC Failure!");
      } else {
        Debug("*** USB MSC Prepared! ***");
      }

      responseMsg("OK:USBMSC Start");
    }
    else if (cmd=='u'){
      Debug("USBMSC End Command");
      SD.endUsbMsc();
      responseMsg("OK:USBMSC End");
    }
    else if (cmd=='F'){
      Debug("File Receive Command");
      CmdFileReceive();
    }
    else if (cmd=='*'){
      Debug("Star Command");
      CmdStar();
      responseMsg("OK:Star");      
    }
    else if (cmd=='.'){
      Debug("Ping Command");
      responseMsg("OK:Ping");      
    }
    else{
      Debug( "Undefined Command :"+String(cmd));
    }

    return cmd;
  }
  return 0;
}

void CmdFileReceive(){
  
  // 注：1KBを超えるファイルを指定すると、Serialで取りこぼすような感じの動作になる
  
  String file_name=Serial.readStringUntil('\n');
  //file_name.replace("\r","");
  file_name.trim();

  Debug("CmdFileReceive filename="+file_name);

  Debug("Wait for SD.begin()");
  while (!SD.begin()) {
    ; /* wait until SD card is mounted. */
  }  
  Debug("confirm SD is mounted.");

  String s_size=Serial.readStringUntil('\n');
  Debug("filesize="+s_size);
  File f;
  f= SD.open(file_name,FILE_WRITE);

  //recvBinaryDataToFile( f, s_size.toInt());
  int nDataSize = s_size.toInt();
  char buff[MAX_READ_BYTES+1];

  String s;
  int readed=0;
  for(int readed=0; readed< nDataSize; ){
    
    int toReadBytes= min(MAX_READ_BYTES, nDataSize - readed );
    Debug("readed="+String(readed)+" toReadBytes="+String(toReadBytes));

    int n = Serial.readBytes(buff,toReadBytes);
    Debug( "serial readed="+String(n) );
    if (n==0){
      Debug(s);
      Debug("zero bytes readed.");
      return;
    }
   
    readed= readed + n;
    
    Debug("f.write buff,"+String(n) );
    int nWriteBytes = f.write(buff,n);
    Debug("wrote bytes to file ="+String(nWriteBytes) );
  }

  f.close();
  String s_end=Serial.readStringUntil('\n');
  Debug("Endmark=" + s_end);
}

void CmdStar(){
  Debug("CmdStar()");
  Debug("Wait for SD.begin()");
  while (!SD.begin()) {
    ; // wait until SD card is mounted. 
  }
  Debug("confirm SD is mounted.");

  File f;
  f= SD.open("123",FILE_WRITE);

  char buff[10];

  int nWriteBytes = f.write(buff,10);
  Debug(" wrote bytes to file ="+String(nWriteBytes) );

  f.close();
}

//uint8_t img_mem[28 * 28] __attribute__((aligned(16)));
DNNVariable dnn_in(28 * 28);
//bool findPiyo = false;

#define WHITE_BALANCE_FRAMES 24
int nframes=0;

void CamCB(CamImage img)
{

  if (!img.isAvailable())
    return;
    
  // Check frame counts for stop auto white balance
  if (nframes == WHITE_BALANCE_FRAMES)
  {
    // Disable auto white balance

    theCamera.setAutoWhiteBalance(false);
  }
  nframes++;

  //とりあえず4フレームおきに処理
  //if (nframes % 4 !=0)
  //  return;

  if (videoFormat== CAM_IMAGE_PIX_FMT_YUV422 ){
    float * pInputData;
    unsigned int i;
  
    // ge.h / cpp はSONYさん製作のラスターオペレーション（画像の便利関数）関数がはいってました。（ジャンケンのサンプルから取得して一部改変）
    // 決め打ちで228, 228 YUV から 28x28のメモリを作っているようですが、ビット操作ではなく、ハードウエアでやっているような？
    // GE.shrink(img, img_mem);
  
    CamImage clip_image;
    //(320x240) -Clip-> (224x224)-Resize-> (28x28)  // ByHW(=By Hardware accelarated function) 
    CamErr err = img.clipAndResizeImageByHW(clip_image,48,8,271,231,28,28);
    if (err) {
      Debug("Error : clipAndResizeImageByHW().");
      return;
    }
  
    if (!clip_image.isAvailable())
      return;
  
    // 入力変数　dnn_in は1次元：要素数28x28で初期化されています。そこに画素データをセットします。
    pInputData = dnn_in.data();
    uint16_t* buf = (uint16_t*)clip_image.getImgBuff();
    for (i = 0; i < 28 * 28; i++)
    {
      pInputData[i] = (float)(*buf);
      buf++;
    }
  
    // DNNRTにセットして・・
    dnnrt.inputVariable(dnn_in, 0);
    // 推論する
    dnnrt.forward();
  
    // 結果の取得
    DNNVariable dnn_out = dnnrt.outputVariable(0);
    // dnn_outは配列になっているので、配列のサイズを取得しておく
    int out_size = dnn_out.size();
  
    String s="DNN Output:";
    for (int i=0; i< out_size; i++){
      char tmp[30];
      sprintf(tmp,"[%d]=%.4f",i,dnn_out[i]);
      if (i!=0){
        s=s+",";
      }
      s = s + String(tmp);
    }
    Debug(s);
  
    //判定イメージの送信
    String img_tag;
    //イメージを変換(RGB565)
    //img.convertPixFormat(CAM_IMAGE_PIX_FMT_RGB565);
    //img_tag="RGB565";
    img.convertPixFormat(CAM_IMAGE_PIX_FMT_GRAY);
    img_tag="GRAY";
  
    uint8_t* send_buf = img.getImgBuff();
    int send_bufSize = img.getImgSize();
    SendImageProperty("ImageStream", 0, img_tag, send_bufSize);  
  
    //データ送信  
    sendBinaryData(send_buf,send_bufSize);
    Debug("DNN Image File transfer end.");   
  
  
    
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
  
    if (videoFormat==CAM_IMAGE_PIX_FMT_YUV422){
      
      //カメラコールバックではフォーマットがYUVに固定されるので何もしない
      
    }
    else if (videoFormat==CAM_IMAGE_PIX_FMT_JPG){

      // reserved.
      
    };
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
  /*
  if (StartCapture()== false){
    Debug("CmdShot() CaptureStart Error ");
    return;
  }
  CamImage img = theCamera.capturePicture();
  */
  CamImage img = theCamera.takePicture();
  if (img.isAvailable()) 
  {
      uint8_t* buf = img.getImgBuff();
      int bufSize = img.getImgSize();
      SendImageProperty("ImageFile", jpegFileNo, "JPEG", bufSize);    
      sendBinaryData(buf,bufSize);
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
    Debug("CmdShot(): takePicture .. img isAvailable()==false ");  
  /*
  img = theCamera.capturePicture();  // 現時点ではStopのために2回必要（captureが１回だとStopに失敗する)
  Debug("CmdShot(): Try Stop Capture. ");  
  if (StopCapture())
    Debug("CmdShot(): Try Stop Capture Success. ");
  else
    Debug("CmdShot(): Try Stop Capture Error. ");
    */
}

void CmdGrayPgm(){
  Debug("CmdGrayPgm()");

  SetCameraMode(cmPgmFile);
  /*
  if (StartCapture()== false){
    Debug("CmdGrayPgm() CaptureStart Error ");
    return;
  }
  */
  
  CamImage img = theCamera.takePicture();
  if (img.isAvailable()) {
    CamImage clip_image;
    //(320x240) -Clip(271-48+1,231-8+1) = (224x224)-Resize-> (28x28)  // ByHW(=By Hardware accelarated function) 
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
    Debug("CmdGrayPgm(): takePicture .. img isAvailable()==false ");  

  /*
  img = theCamera.capturePicture();  // 現時点ではStopのために2回必要（captureが１回だとStopに失敗する)
  Debug("CmdGrayPgm(): Try Stop Capture. ");  
  if (StopCapture())
    Debug("CmdGrayPgm(): Try Stop Capture Success. ");
  else
    Debug("CmdGrayPgm(): Try Stop Capture Error. ");
  */
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
  sendBinaryData(buf,bufSize);
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
    startDNNRT();
    Debug("CmdDNN() START CameraCallback for DNN");
    nframes=0;
    theCamera.startStreaming(true, CamCB);
  }
  else{
    endDNNRT();
    Debug("CmdDNN() STOP CameraCallback for DNN");
    theCamera.startStreaming(false, CamCB);   
  }
}

//Override Functions of Arduino------------------------------------------------------

void startDNNRT()
{
  if (isDNNReady)
    endDNNRT();
    
  /* DNNRTを初期化 */
  digitalWrite(LED_DNN, HIGH);
  Debug("Loading DNN file");
 
  File nnbfile("sanbiki.nnb");
  if (!nnbfile) {
    Debug("DNN File Not found.");
    return;
  }

  Debug("Initialize DNNRT");
  int ret = dnnrt.begin(nnbfile);
  if (ret < 0){
    Debug("DNNRT initialize error.");
  }
  isDNNReady=true;
}

void endDNNRT()
{
  Debug("Finalize DNNRT");
  if (isDNNReady){
    dnnrt.end();
    isDNNReady=false;   
  }
}

void setup() 
{
  digitalWrite(LED_SERIAL, HIGH);
  
  //Serialを利用可能に
  Serial.begin(BAUDRATE);
  while (!Serial) {};

  digitalWrite(LED_CAMERA, HIGH);

  //カメラを利用可能に
  theCamera.begin(
    1,                // Buffer Num (int)
    CAM_VIDEO_FPS_30, // FPS (CAM_VIDEO_FPS)
    CAM_IMGSIZE_QVGA_H, // video_width (int)
    CAM_IMGSIZE_QVGA_V, // video_height (int)
    videoFormat  // video format(設定用の定数)
  );

  theCamera.setAutoWhiteBalanceMode(CAM_WHITE_BALANCE_DAYLIGHT);

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
