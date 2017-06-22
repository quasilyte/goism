package conformance

func add1Int(x int) int      { return x + 1 }
func addInt(x, y, z int) int { return x + y + z }
func sub1Int(x int) int      { return x - 1 }
func subInt(x, y, z int) int { return x - y - z }
func mulInt(x, y, z int) int { return x * y * z }
func quoInt(x, y, z int) int { return x / y / z }
func gtInt(x, y int) bool    { return x > y }
func ltInt(x, y int) bool    { return x < y }

func add1Float(x float64) float64      { return x + 1 }
func addFloat(x, y, z float64) float64 { return x + y + z }
func sub1Float(x float64) float64      { return x - 1 }
func subFloat(x, y, z float64) float64 { return x - y - z }
func mulFloat(x, y, z float64) float64 { return x * y * z }
func quoFloat(x, y, z float64) float64 { return x / y / z }
func gtFloat(x, y float64) bool        { return x > y }
func ltFloat(x, y float64) bool        { return x < y }

func concatStr(x, y, z string) string { return x + y + z }
func ltStr(x, y string) bool          { return x < y }
