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


  def printTree(file: File, indent: String = "") :Unit = {
    if (file.isDirectory()) {
      printBranch(ls(file), indent)
    }
  }

  @tailrec
  def printBranch(files: List[File], indent: String): Unit = {
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
//    tree(new File(args(0)))
    printTree(new File(args(0)))
    println
    println("%d directories, %d files".format(dirNum, fileNum))
  }
}
