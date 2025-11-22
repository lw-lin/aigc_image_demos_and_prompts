import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.{Try, Success, Failure}
import scala.util.control.Breaks._

/**
   * 我们对 details 目录里的数据做如下设计：
   * 1、details 目录里，只能有子文件夹（记为 1 级子目录）、不能有子文件；每个 1 级子目录里，只能有子文件夹（记为 2 级子目录）、不能有子文件
   * 2、2 级子目录里，必须有 prompts_and_details.md 文件；且文件内容里，必须且只能有：source、prompt、extra_info 这 3 个 #一级标题
   * 3、2 级子目录里，必须有一个图片文件，它的名字是 img，扩展名限制为 jpg、jpeg、png、gif、webp
   * 4、2 级子目录里，如果有其它文件，必须以 extra_ 开头
   * 
   * 本程序的主要功能是：
   * 1、检查 details 目录里的数据是否合法
   * 2、如果合法，则生成 tableData JSON，并更新 index.html 文件；具体：1 级子目录的名称，将作为表格的左侧列的分类名称；2 级子目录的名称，将作为表格里 desc 的具体取值
   * 3、如果不合法，则打印错误信息
   */
object UpdateHtmlMain {
  val detailsDir = new File("details")
  val indexHtmlPath = "index.html"
  
  // 支持的图片扩展名
  val imageExtensions = Set(".jpg", ".jpeg", ".png", ".gif", ".webp")
  

  def main(args: Array[String]): Unit = {
    println("开始生成 tableData...")
    checkDataDir()
    updateIndexHtml()
    println("完成！")
  }

  /**
   * 按照我们对 details 目录里的数据设计，检查数据是否合法
   */
  def checkDataDir(): Unit = {
    if (!detailsDir.exists() || !detailsDir.isDirectory) {
      throw new RuntimeException("错误：details 目录不存在")
    }
    
    var hasError = false
    
    // 检查 1 级子目录
    val level1Dirs = detailsDir.listFiles().filter(_.isDirectory).toList
    val level1Files = detailsDir.listFiles().filter(_.isFile).toList
    
    // 1. 检查 details 目录下不能有文件
    if (level1Files.nonEmpty) {
      println(s"错误：details 目录下不能有文件，但发现了：${level1Files.map(_.getName).mkString(", ")}")
      hasError = true
    }
    
    // 遍历每个 1 级子目录
    level1Dirs.foreach { level1Dir =>
      val level1Name = level1Dir.getName
      
      // 检查 1 级子目录下不能有文件，只能有子文件夹（2级子目录）
      val level1Files = level1Dir.listFiles().filter(_.isFile).toList
      if (level1Files.nonEmpty) {
        println(s"错误：1 级子目录 '$level1Name' 下不能有文件，但发现了：${level1Files.map(_.getName).mkString(", ")}")
        hasError = true
      }
      
      // 获取 2 级子目录
      val level2Dirs = level1Dir.listFiles().filter(_.isDirectory).toList
      
      // 遍历每个 2 级子目录
      level2Dirs.foreach { level2Dir =>
        val level2Name = level2Dir.getName
        val fullPath = s"$level1Name/$level2Name"
        
        // 获取 2 级子目录下的所有文件
        val allFiles = level2Dir.listFiles().filter(_.isFile).toList
        
        // 2. 检查 prompts_and_details.md 文件
        val mdFile = new File(level2Dir, "prompts_and_details.md")
        if (!mdFile.exists()) {
          println(s"错误：2 级子目录 '$fullPath' 中缺少 prompts_and_details.md 文件")
          hasError = true
        } else {
          // 检查 markdown 文件内容
          checkMarkdownContent(mdFile, fullPath) match {
            case Some(error) =>
              println(error)
              hasError = true
            case None => // 检查通过
          }
        }
        
        // 3. 检查图片文件（img.*，扩展名限制为 jpg、jpeg、png、gif、webp）
        val imgFileNames = imageExtensions.map(ext => s"img$ext")
        val imgFiles = imgFileNames.map(name => new File(level2Dir, name))
        val foundImgFile = imgFiles.find(_.exists())
        if (foundImgFile.isEmpty) {
          val allowedExts = imageExtensions.mkString("、")
          println(s"错误：2 级子目录 '$fullPath' 中缺少 img.* 格式的图片文件（扩展名限制为：$allowedExts）")
          hasError = true
        }
        
        // 4. 检查其他文件是否以 extra_ 开头
        val requiredFiles = Set("prompts_and_details.md") ++ imgFileNames
        val otherFiles = allFiles.filter(f => !requiredFiles.contains(f.getName))
        
        otherFiles.foreach { file =>
          if (!file.getName.startsWith("extra_")) {
            println(s"错误：2 级子目录 '$fullPath' 中的文件 '${file.getName}' 必须以 extra_ 开头")
            hasError = true
          }
        }
      }
    }
    
    if (hasError) {
      throw new RuntimeException("数据目录检查失败，请修复上述错误后重试")
    } else {
      println("数据目录检查通过")
    }
  }
  
  // 检查 markdown 文件内容是否包含且仅包含 source、prompt、extra_info 三个一级标题
  def checkMarkdownContent(mdFile: File, dirPath: String): Option[String] = {
    Try {
      val content = Source.fromFile(mdFile, "UTF-8").mkString
      val lines = content.split("\n").toList
      
      // 查找所有一级标题（以 # 开头，后面跟空格，然后是标题文本）
      val h1Pattern = """^#\s+(.+)$""".r
      val h1Titles = lines.collect {
        case h1Pattern(title) => title.trim.toLowerCase
      }
      
      val requiredTitles = Set("source", "prompt", "extra_info")
      val foundTitles = h1Titles.toSet
      
      // 检查是否包含所有必需的标题
      val missingTitles = requiredTitles -- foundTitles
      if (missingTitles.nonEmpty) {
        Some(s"错误：2 级子目录 '$dirPath' 的 prompts_and_details.md 中缺少一级标题：${missingTitles.mkString(", ")}")
      } else {
        // 检查是否只有这三个标题（允许重复，但只允许这三个）
        val extraTitles = foundTitles -- requiredTitles
        if (extraTitles.nonEmpty) {
          Some(s"错误：2 级子目录 '$dirPath' 的 prompts_and_details.md 中只能有 source、prompt、extra_info 三个一级标题，但发现了：${extraTitles.mkString(", ")}")
        } else {
          None
        }
      }
    } match {
      case Success(result) => result
      case Failure(e) => Some(s"错误：读取 2 级子目录 '$dirPath' 的 prompts_and_details.md 文件失败：${e.getMessage}")
    }
  }

  // 从 markdown 文件中提取 source、prompt、extra_info 三个部分
  def extractMarkdownSections(mdFile: File): (String, String, String) = {
    if (!mdFile.exists()) {
      return ("", "", "")
    }
    
    Try {
      val content = Source.fromFile(mdFile, "UTF-8").mkString
      val lines = content.split("\n").toList
      
      // 提取 source 部分
      val source = extractSection(lines, "# source", "# prompt")
      
      // 提取 prompt 部分
      val prompt = extractSection(lines, "# prompt", "# extra_info")
      
      // 提取 extra_info 部分
      val extraInfo = extractSection(lines, "# extra_info", null)
      
      (source, prompt, extraInfo)
    } match {
      case Success(result) => result
      case Failure(_) => ("", "", "")
    }
  }
  
  // 提取两个标题之间的内容
  def extractSection(lines: List[String], startMarker: String, endMarker: String | Null): String = {
    val startIndex = lines.indexWhere(_.trim.toLowerCase == startMarker.toLowerCase)
    if (startIndex < 0) {
      return ""
    }
    
    val endIndex = if (endMarker != null && endMarker.nonEmpty) {
      lines.indexWhere(_.trim.toLowerCase == endMarker.toLowerCase, startIndex + 1)
    } else {
      -1
    }
    
    val sectionLines = if (endIndex > 0) {
      lines.slice(startIndex + 1, endIndex)
    } else {
      lines.drop(startIndex + 1)
    }
    
    sectionLines
      .map(_.trim)
      .filter(_.nonEmpty)
      .mkString("\n")
      .trim
  }
  
  // 查找目录中的图片文件（查找 img.* 格式，扩展名限制为 jpg、jpeg、png、gif、webp）
  def findImageFile(dir: File): Option[File] = {
    if (!dir.exists() || !dir.isDirectory) {
      return None
    }
    
    dir.listFiles()
      .find { file =>
        file.isFile && {
          val name = file.getName.toLowerCase
          name.startsWith("img.") && imageExtensions.exists { ext =>
            name == s"img$ext"
          }
        }
      }
  }
  
  // 生成 tableData JSON
  def generateTableData(): String = {
    if (!detailsDir.exists() || !detailsDir.isDirectory) {
      return """{
            "rows": []
        }"""
    }
    
    val rows = detailsDir.listFiles()
      .filter(_.isDirectory)
      .sortBy(_.getName)
      .map { level1Dir =>
        val category = level1Dir.getName
        
        // 获取 2 级子目录
        val level2Dirs = level1Dir.listFiles()
          .filter(_.isDirectory)
          .sortBy(_.getName)
          .toList
        
        // 收集所有 cells（2 级目录下的图片）
        val cells = scala.collection.mutable.ListBuffer[Map[String, Any]]()
        
        // 遍历每个 2 级子目录
        level2Dirs.foreach { level2Dir =>
          val level2Name = level2Dir.getName
          
          // 查找 2 级目录下的图片和 markdown
          val level2Image = findImageFile(level2Dir)
          val level2Md = new File(level2Dir, "prompts_and_details.md")
          val (source, prompt, extraInfo) = extractMarkdownSections(level2Md)
          
          // 如果 2 级目录有图片，添加到 cells
          // 2 级子目录的名称作为 desc 的值
          level2Image.foreach { img =>
            val relativePath = s"details/${category}/${level2Name}/${img.getName}"
            cells += Map(
              "image" -> relativePath,
              "desc" -> level2Name,  // 使用 2 级子目录的名称作为 desc
              "tooltip" -> Map(
                "source" -> s"【source】$source",
                "prompt" -> s"【prompt】$prompt",
                "extra_info" -> s"【extra_info】$extraInfo"
              )
            )
          }
        }
        
        Map(
          "category" -> category,
          "cells" -> cells.toList
        )
      }
      .filter(row => row("cells").asInstanceOf[List[?]].nonEmpty) // 只保留有 cells 的行
      .toList
    
    // 转换为 JSON 字符串
    val rowsJson = rows.map { row =>
      val category = row("category").asInstanceOf[String]
      val cells = row("cells").asInstanceOf[List[Map[String, Any]]]
      
      val cellsJson = cells.map { cell =>
        val image = cell("image").asInstanceOf[String]
        val desc = cell("desc").asInstanceOf[String]
        val tooltip = cell("tooltip").asInstanceOf[Map[String, String]]
        val source = tooltip("source")
        val prompt = tooltip("prompt")
        val extraInfo = tooltip("extra_info")
        
        s"""                        { "image": ${escapeJson(image)}, "desc": ${escapeJson(desc)}, "tooltip": { "source": ${escapeJson(source)}, "prompt": ${escapeJson(prompt)}, "extra_info": ${escapeJson(extraInfo)} } }"""
      }.mkString(",\n")
      
      s"""                {
                    "category": "$category",
                    "cells": [
$cellsJson
                    ]
                }"""
    }.mkString(",\n")
    
    s"""const tableData = {
            "rows": [
$rowsJson
            ]
        };"""
  }
  
  // 转义 JSON 字符串（注意顺序：先转义反斜杠）
  def escapeJson(str: String): String = {
    "\"" + str
      .replace("\\", "\\\\")  // 先转义反斜杠
      .replace("\"", "\\\"")  // 再转义引号
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
      + "\""
  }
  
  // 更新 index.html
  def updateIndexHtml(): Unit = {
    val htmlContent = Source.fromFile(indexHtmlPath, "UTF-8").mkString
    
    // 查找 tableData 的开始和结束位置
    val startPattern = "const tableData = {"
    
    val startIndex = htmlContent.indexOf(startPattern)
    if (startIndex == -1) {
      println("错误：在 index.html 中找不到 tableData 的开始位置")
      return
    }
    
    // 找到对应的结束位置（查找匹配的 };）
    var braceCount = 0
    var foundStart = false
    var endIndex = startIndex
    
    breakable {
      for (i <- startIndex until htmlContent.length) {
        val char = htmlContent(i)
        if (char == '{') {
          braceCount += 1
          foundStart = true
        } else if (char == '}') {
          braceCount -= 1
          if (foundStart && braceCount == 0) {
            // 检查后面是否有分号
            var j = i + 1
            while (j < htmlContent.length && htmlContent(j).isWhitespace) {
              j += 1
            }
            if (j < htmlContent.length && htmlContent(j) == ';') {
              endIndex = j + 1
              break
            }
          }
        }
      }
    }
    
    if (endIndex == startIndex) {
      println("错误：在 index.html 中找不到 tableData 的结束位置")
      return
    }
    
    val newTableData = generateTableData()
    val updatedContent = htmlContent.substring(0, startIndex) + 
                        newTableData + 
                        htmlContent.substring(endIndex)
    
    // 写入文件
    val writer = new PrintWriter(indexHtmlPath, "UTF-8")
    try {
      writer.write(updatedContent)
      println(s"成功更新 $indexHtmlPath")
    } finally {
      writer.close()
    }
  }
}
