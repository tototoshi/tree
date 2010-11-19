package tree

import java.io.File
import scala.annotation.tailrec

object Tree extends Application {

  /*
   * コマンド結果の描画に使うパーツ群
   */
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
  def ls(dir: File) :Option[List[File]] = {
    try {
      Some(dir.listFiles.toList.filterNot(_.getName.startsWith(".")))
    } catch {
      // パーミッションがないディレクトリに対してlistFiles()を適用するとNullが飛ぶ
      case e: NullPointerException => println("Permission Denied. [%s]".format(dir))
      None
    }
  }

  /**
   * ディレクトリーツリーを表示する。
   *
   * @param dir ディレクトリ
   * @param indent ツリー表示用のインデント。ユーザが使うことはない。
   * @return Unit
   */
  def printTree(dir: File, indent: String = "") :Unit = {
      printBranch(ls(dir).getOrElse(Nil), indent)
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
    files.length match {
      case 0 => Unit
      case 1 => {
	println(indent + \--  + files.head.getName)
	if (files.head.isDirectory) {
          printTree(files.head, indent + ____)
	}
      }
      case _ => {
	println(indent + |-- + files.head.getName)
	if (files.head.isDirectory) {
          printTree(files.head, indent + |)
	}
	printBranch(files.tail, indent)
      }
    }
  }

  /**
   * main メソッド
   * 引数でツリーのルートとなるディレクトリを指定する
   * 指定のないときはカレントディレクトリをルートとする。
   *
   * 引数としてディレクトリ以外が指定されるとエラーとなる。
   *
   * @param args ディレクトリ
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
