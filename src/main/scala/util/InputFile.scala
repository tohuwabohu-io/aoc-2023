package util

import scala.io.Source

def input(filePath: String) = 
  Source.fromResource(filePath).getLines().to(LazyList)
