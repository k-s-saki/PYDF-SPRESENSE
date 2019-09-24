プログラム　SPCAM_USB

USBを通して、Spresenseのカメラ機能をコントロールします。


ソースコード
2019/9/24　FreePascal3.0.4/Lazarus2.0.4で作成

クロスプラットホームのPascalコンパイラ＋IDE＋ネイティブGUIクラスライブラリです。LazarusをインストールすればFreePascalは自動的にインストールされます。
https://sourceforge.net/projects/lazarus/files/

ARM系Linux（Raspberry Pi)、Macでもコンパイルできるはずですが、
ビットマップへの展開はOSに依存しています。
Windows(64bit/Win10)で製作していますので、WindowsではB->R->Gの順です。

TBlockSerialについて
クロスプラットホームのRS232Cライブラリです。
プロジェクト内（AraratSynapse-ultiboフォルダ）
にソースを配置していますので、別途インストールする
必要は無いと思われます。

ダウンロード先
https://github.com/ultibohub/AraratSynapse

AraratSynapse-utilbo（ドキュメント）
http://synapse.ararat.cz/doc/help/synaser.TBlockSerial.html

