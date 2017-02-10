object Dijkstra {
 
  type Path[Key] = (Double, List[Key])
 
  @tailrec
  def Dijkstra[Key](lookup: Map[Key, List[(Double, Key)]], 
					fringe: List[Path[Key]], dest: Key, visited: Set[Key]): Path[Key] = {
	  fringe match {
		case (dist, path) :: fringe_rest => path match {
		  case key :: path_rest =>
		    if (key == dest) (dist, path.reverse)
		    else {
			  val paths = lookup(key).flatMap {
			    case (d, key) => 
				  if (!visited.contains(key)) List((dist + d, key :: path)) else Nil
			  }
			  
			  val sorted_fringe = (paths ++ fringe_rest).sortWith {case ((d1, _), (d2, _)) => d1 < d2}
			  Dijkstra(lookup, sorted_fringe, dest, visited + key)
		    }
		}
    
		case Nil => (0, List())
	}
  }