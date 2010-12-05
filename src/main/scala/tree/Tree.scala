package tree

import java.io.File
import scala.annotation.tailrec

object Tree {

  /*
   * コマンド結果の描画に使うパーツ群
   */
  private val ____ = "    "
  private val |-- = "|-- "
  private val |   = "|   "
  private val \--   = "`-- "

  /**
   * Linuxのlsコマンドのように特定のディレクトリ内のファイル一覧を取得する
   * 隠しファイル、カレントディレクトリ、親ディレクトリは表示しない。
   *
   * @param dir ディレクトリ
   * @return ディレクトリ内のファイル一覧
   */
  private def ls(dir: File) :List[File] = {
    try {
      dir.listFiles.toList.filterNot(_.getName.startsWith("."))
    } catch {
      // パーミッションがないディレクトリに対してlistFiles()を適用するとNullが飛ぶ
      case e: NullPointerException => println("Permission Denied. [%s]".format(dir))
      Nil
    }
  }

  /**
   * ディレクトリーツリーを表示する。
   *
   * @param dir ディレクトリ
   * @param indent ツリー表示用のインデント。ユーザが使うことはない。
   * @return Unit
   */
  private def printTree(dir: File, indent: String = "") :Unit = {
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
  private def printBranch(files: List[File], indent: String): Unit = {
    files.length match {
      case 0 => ()
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
  def main(args: Array[String]): Unit = {
    val dir = new File(if (args.length == 0) "."
		       else args(0))

    if (!dir.isDirectory()) {
      println("[Error] " + dir + " is not directory!")
      exit(1)
    }

    printTree(dir)
  }
}
