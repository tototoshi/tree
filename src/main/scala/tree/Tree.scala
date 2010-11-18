package tree

import java.io.File

object Tree extends Application {
  val indent = "    "
  val branch = "|-- "
  val trunk  = "|   "
  val edge   = "`-- "
  private var dirNum = 0;
  private var fileNum = 0;

  /**
   * Linuxのlsコマンドのように特定のディレクトリ内のファイル一覧を取得する
   * 隠しファイル、カレントディレクトリ、親ディレクトリは表示しない。
   *
   * @param dir ディレクトリ
   * @return ディレクトリ内のファイル一覧
   */
  def ls(dir: File) :List[File] =
    dir.listFiles.toList.remove(_.getName.startsWith("."))

  private def tree(dir: File, branch: String): List[String] = {
    var curStr:List[String] = Nil
    val files = ls(dir).reverse

    for (i <- 0 until files.length) {
      val name = files(i).getName
      val curBranch1 = if (i == files.length - 1) indent
                       else trunk
      val curBranch2 = if (i == files.length - 1) "`-- "
                       else "|-- "

      files(i) match {
        case f if f.isDirectory() => {
	  dirNum += 1
          curStr = tree(files(i), branch + curBranch1) :::
          ((branch + curBranch2 + name) :: curStr)
	}
	case _ => {
          fileNum += 1
          curStr = (branch + curBranch2 + name) :: curStr
	}
      }
    }
    curStr
  }


  /**
   *  main メソッド
   *
   * @param args
   */
  override def main(args: Array[String]): Unit = {
    try {
      val treeList = tree(new File(args(0)), "") ::: List(".")
      println(treeList.reverse.mkString("\n"))
      println
      println("%d directories, %d files".format(dirNum, fileNum))
    } catch {
      case e: NullPointerException => println("[error: cannot open directory]")
    }
  }
}
