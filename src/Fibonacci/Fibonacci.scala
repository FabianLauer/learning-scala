object Fibonacci {
	def main(args: Array[String]): Unit = {
		var prevNum = 1;
		var num: Int = 1;
		for (i <- 1 to 20) {
			var next = prevNum + num;
			println(num);
			prevNum = num;
			num = next;
		}
	}
}