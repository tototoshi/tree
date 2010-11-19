package tree

import java.io.File
import scala.annotation.tailrec

object Tree extends Application {
  private var dirNum = 0;
  private var fileNum = 0;
  val offset = "    "
  val branch = "|-- "
  val trunk  = "|   "
  val edge   = "`-- "

  /**
   * Linuxのlsコマンドのように特定のディレクトリ内のファイル一覧を取得する
   * 隠しファイル、カレントディレクトリ、親ディレクトリは表示しない。
   *
   * @param dir ディレクトリ
   * @return ディレクトリ内のファイル一覧
   */
  def ls(dir: File) :List[File] =
    dir.listFiles.toList.filterNot(_.getName.startsWith("."))

  /**
   * ディレクトリーツリーを表示する。
   *
   * @param dir ディレクトリ
   * @param indent ツリー表示用のインデント。ユーザが使うことはない。
   * @return Unit
   */
  def printTree(dir: File, indent: String = "") :Unit = {
      printBranch(ls(dir), indent)
  }

  /**
   * ディレクトリツリーの枝の一本一本を再帰的に描画する。
   *
   * @param files ファイルのリスト
   * @param indent ツリー表示用のインデント。ユーザが使うことはない。
   * @return Unit
   *
   */
  @tailrec
  def printBranch(files: List[File], indent: String): Unit = {
    // TODO ifばっかりで汚いのであとでリファクタ
    if (files.length == 0){
      Unit
    } else if (files.length == 1) {
      println(indent + edge  + files.head.getName)
      if (files.head.isDirectory) {
        printTree(files.head, indent + offset)
      }
    } else {
      println(indent + branch + files.head.getName)
      if (files.head.isDirectory) {
        printTree(files.head, indent + trunk)
      }
      printBranch(files.tail, indent)
    }

  }

  /**
   *  main メソッド
   *
   * @param args
   */
  override def main(args: Array[String]): Unit = {
    val dir = new File(args(0))
    if (!dir.isDirectory()) {
      println("[Error] " + dir + " is not directory!")
      exit(1)
    }
    printTree(new File(args(0)))
//    println
//    println("%d directories, %d files".format(dirNum, fileNum))
  }
}
