package tree

import java.io.File
import scala.annotation.tailrec

object Tree extends Application {
  val ____ = "    "
  val |-- = "|-- "
  val |   = "|   "
  val \--   = "`-- "

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
      println(indent + \--  + files.head.getName)
      if (files.head.isDirectory) {
        printTree(files.head, indent + ____)
      }
    } else {
      println(indent + |-- + files.head.getName)
      if (files.head.isDirectory) {
        printTree(files.head, indent + |)
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
    val dir = new File(args.length match {
      case 0 => "."
      case _ => args(0)
    })
    if (!dir.isDirectory()) {
      println("[Error] " + dir + " is not directory!")
      exit(1)
    }
    printTree(dir)
  }
}
