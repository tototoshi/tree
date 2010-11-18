package tree

import java.io.File

object Tree extends Application {
  val offset = "    "
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
    dir.listFiles.toList.filterNot(_.getName.startsWith("."))

  /**
   * コンソールに出力する文字列が入ったリストを返す
   *
   * @param dir ディレクトリ
   * @param indent インデント(ディレクトリ階層を潜っていくと積み上げられていく
   * @return コンソールに出力する文字列が入ったリスト
   */
  private def tree(dir: File, indent: String): List[String] = {
    var curStr:List[String] = Nil
    val files = ls(dir).reverse

    for (i <- 0 until files.length) {
      val name = files(i).getName
      val curBranch1 = if (i == files.length - 1) offset
                       else trunk
      val curBranch2 = if (i == files.length - 1) edge
                       else branch

      files(i) match {
        case f if f.isDirectory => {
	  dirNum += 1
          curStr = tree(files(i), indent + curBranch1) :::
          ((indent + curBranch2 + name) :: curStr)
	}
	case _ => {
          fileNum += 1
          curStr = (indent + curBranch2 + name) :: curStr
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
