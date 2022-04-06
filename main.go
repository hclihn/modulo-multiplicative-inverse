package main

import (
	"fmt"
  "math"
  "math/big"
  "strings"
  "encoding/hex"
  "regexp"
  "runtime"
)

func GenWorker(p uint) func(id int, result chan *big.Float) {
	B1 := new(big.Float).SetPrec(p).SetInt64(1)
	B2 := new(big.Float).SetPrec(p).SetInt64(2)
	B4 := new(big.Float).SetPrec(p).SetInt64(4)
	B5 := new(big.Float).SetPrec(p).SetInt64(5)
	B6 := new(big.Float).SetPrec(p).SetInt64(6)
	B8 := new(big.Float).SetPrec(p).SetInt64(8)
	B16 := new(big.Float).SetPrec(p).SetInt64(16)

	return func(id int, result chan *big.Float) {
		Bn := new(big.Float).SetPrec(p).SetInt64(int64(id))

		C1 := new(big.Float).SetPrec(p).SetInt64(1)
		for i := 0; i < id; i++ {
			C1.Mul(C1, B16)
		}

		C2 := new(big.Float).SetPrec(p).Mul(B8, Bn)

		T1 := new(big.Float).SetPrec(p).Add(C2, B1)
		T1.Quo(B4, T1)

		T2 := new(big.Float).SetPrec(p).Add(C2, B4)
		T2.Quo(B2, T2)

		T3 := new(big.Float).SetPrec(p).Add(C2, B5)
		T3.Quo(B1, T3)

		T4 := new(big.Float).SetPrec(p).Add(C2, B6)
		T4.Quo(B1, T4)

		R := new(big.Float).SetPrec(p).Sub(T1, T2)
		R.Sub(R, T3).Sub(R, T4).Quo(R, C1)

		result <- R
	}
}

func GetBuf(nBytes int) []byte {
	runtime.GOMAXPROCS(runtime.NumCPU())
  
  nb := 8 * nBytes
  p := uint(nb + 32)
  n := nb / 4 + 1// hex digits

	result := make(chan *big.Float, n)
	worker := GenWorker(p)

	pi := new(big.Float).SetPrec(p).SetInt64(0)

	for i := 0; i < n; i++ {
		go worker(i, result)
	}

	for i := 0; i < n; i++ {
		pi.Add(pi, <-result)
	}

  xx := new(big.Float).SetMantExp(pi, nb-pi.MantExp(nil))
  x, _ := xx.Int(nil)
  buf := make([]byte, nBytes)
  x.FillBytes(buf)
  return buf
}

func H2oMarkByte(t, idx int) uint8 {
  const nTypes = 4
  t, idx = t % nTypes, idx % 8
  switch idx {
    case 0:
      switch t {
        case 0:
          return 0x0c
        case 1:
          return 0x0d
        case 2:
          return 0x05
        case 3:
          return 0x0b
      }
    case 1:
      switch t {
        case 0:
          return 0x0a
        case 1:
          return 0x0e
        case 2:
          return 0x0e
        case 3:
          return 0x0a
      }
    case 2:
      switch t {
        case 0:
          return 0x0b
        case 1:
          return 0x0c
        case 2:
          return 0x0e
        case 3:
          return 0x05
      }
    case 3:
      switch t {
        case 0:
          return 0x00
        case 1:
          return 0x0a
        case 2:
          return 0x0d
        case 3:
          return 0x01
      }
    case 4:
      switch t {
        case 0:
          return 0x00
        case 1:
          return 0x0f
        case 2:
          return 0x01
        case 3:
          return 0x01
      }
    case 5:
      switch t {
        case 0:
          return 0x0d
        case 1:
          return 0x0f
        case 2:
          return 0x0e
        case 3:
          return 0x07
      }
    case 6:
      switch t {
        case 0:
          return 0x01
        case 1:
          return 0x0e
        case 2:
          return 0x05
        case 3:
          return 0x0e
      }
    case 7:
      switch t {
        case 0:
          return 0x0e
        case 1:
          return 0x0d
        case 2:
          return 0x05
        case 3:
          return 0x0a
      }
  }
  return 0xff
}

func H2oMarkPtn(mType, i int) uint8 {
  q, r := (mType + i*i) / 8, i % 8
  b := H2oMarkByte(q, r) & 0x0f
  br := (^b) & 0x0f
  switch r {
  case 0:
    return b
  case 1:
    return b << 4
  case 2:
    return b | (b << 4)
  case 3:
    return (b << 4) | br
  case 4:
    return (br << 4) | b
  case 5:
    return br | (br << 4)
  case 6:
    return br
  case 7:
    return (br << 4)
  }
  return b
}

func H2oMarkBuf(buf []byte, mType int) []byte {
  bufo := make([]byte, len(buf))
  for i := range buf {
    bufo[i] = buf[i] ^ H2oMarkPtn(mType, i)
  }
  return bufo
}

func CreateMxy(n int) (m, x, y *big.Int) {
  two := big.NewInt(2)
  nb := int64(n * 8)
  m = new(big.Int).Exp(two, big.NewInt(nb), nil)
  m.Sub(m, big.NewInt(1))
  x = new(big.Int).SetBytes(H2oMarkBuf(GetBuf(n), 0))
  if x.Bit(0) == 0 { // even
    x.Add(x, big.NewInt(1))
  }
  y = new(big.Int)
  for {
    p := y.ModInverse(x, m)
  	fmt.Printf("--> m: %x, x: %x, y: %x\n", m, x, y)
    if p !=  nil && y.Cmp(x) != 0 {
      break
    }
    x.Add(x, two)
  }
  return
}

func sqrt(x int) int {
	return int(math.Sqrt(float64(x)))
}

func encode4(x int) int {
	sqrtR := sqrt((x * x) % 16)
	d := x - sqrtR
	switch sqrtR {
	case 0, 2:
		return ((sqrtR << 2) & 0x0c) | (d / 4)
	case 1:
		y := d / 2
		yq, yr := y/4, y%4
		return ((sqrtR << 2) & 0x0c) | (yq ^ yr)
	case 3:
		y := d / 2
		yq, yr := y/4, y%4
		return ((sqrtR << 2) & 0x0c) | ((yq << 1) ^ yr)
	}
	return 0
}

func decode4(x int) int {
	sqrtR := (x & 0x0c) >> 2
	lower := x & 0x03

	switch sqrtR {
	case 0, 2:
		return lower*4 + sqrtR
	case 1:
		b := (lower&0x01) ^ ((lower&0x02)>>1)
		b |= (b << 2)
		return (lower^b)*2 + sqrtR
	case 3:
		y := (lower&0x02)<<1 + (lower & 0x01)
		return y*2 + sqrtR
	}
	return 0
}

func encodeByte(x byte) byte {
  u, l := int(x) & 0x0f, (int(x) & 0xf0) >> 4
  ecu, ecl := encode4(u), encode4(l)
  return byte(((ecu << 4) & 0xf0) | (ecl & 0x0f)) 
}

func decodeByte(x byte) byte {
  u, l := int(x) & 0x0f, (int(x) & 0xf0) >> 4
  ecu, ecl := decode4(u), decode4(l)
  return byte(((ecu << 4) & 0xf0) | (ecl & 0x0f)) 
}

func ToU(buf []byte, bv, bg int, m, x *big.Int, ec func(byte) byte) []byte {
  l := len(buf)
  u := new(big.Int)
  bufv := make([]byte, 0)
  bufi := make([]byte, bv)
  bufo := make([]byte, bg)
  
  for i := 0; i < l; i += bv {
    for j := 0; j < bv; j++ {
      k := i + j
      if k < l {
        bufi[j] = buf[k]
      } else {
        bufi[j] = 0x00 // pad
      }
      bufi[j] ^= H2oMarkPtn(bv, k)
      if k % 2 == 1 {
        bufi[j] = ec(bufi[j])
      }
    }
    u.SetBytes(bufi).Mul(u, x).Mod(u, m).FillBytes(bufo)
    bufv = append(bufv, bufo...)
  }
  for i := range bufv {
    if i % 2 == 1 {
      bufv[i] = ec(bufv[i])
    }
    bufv[i] ^= H2oMarkPtn(bg, i)
  }
  return bufv
}

func ToCode(buf []byte, name string) string {
  strs := make([]string, len(buf))
  for j := 0; j < len(buf); j++ {
    strs[j] = fmt.Sprintf("0x%02x", buf[j])
  }
  return fmt.Sprintf("var %s = [%d]byte{ %s }", name, len(strs), strings.Join(strs, ", "))
}

func Compare(ori, out []byte) {
  li, lo := len(ori), len(out)
  if li != lo {
    fmt.Printf("ERROR: mismatch original length (%d) with decoded (%d): ori=% 02x , out=% 02x \n", li, lo, ori, out)
    return
  }
  hasErr := false
  for i, iv := range ori {
    ov := out[i]
    if iv != ov {
      fmt.Printf("ERROR: mismatch ori[%d] (0x%02x) with decoded (0x%02x)\n", i, iv, ov)
      hasErr = true
    }
  }
  if !hasErr {
    fmt.Printf("Original matches decoded!\n")
  }
  return
}

func HexToBytes(s string) ([]byte, error) {
  fmt.Printf("\nConverting %q to hex bytes...\n", s)
  re := regexp.MustCompile(`[^0-9a-fA-F]+`)
	src := []byte(re.ReplaceAllLiteralString(s, ""))
  
  dst := make([]byte, hex.DecodedLen(len(src)))
	n, err := hex.Decode(dst, src)
	if err != nil {
		return nil, fmt.Errorf("failed to decode hex string %q: %w", string(src), err)
	}
  return dst[:n], nil
}

func main() {
  const N = 16
  const V = 8
  m, x, y := CreateMxy(N)

    pb, err := HexToBytes("de6932367d733afb550c47cd8a4d06080b8ddfa7cb7c55b1f2454f8361e6ba9d")
  if err != nil {
    fmt.Printf("ERROR: failed to convert pb: %s!\n", err)
    return
  }
  pbu := ToU(pb, V, N, m, x, encodeByte)
  fmt.Printf("%s\n==> %s\n", ToCode(pb, "pb"), ToCode(pbu, "pbu"))

  ub, err := HexToBytes("39558dac3e8bef848921e12baee6591b6645f33ceb4b022d8a2b897fcfbdffb4")
  if err != nil {
    fmt.Printf("ERROR: failed to convert ub: %s!\n", err)
    return
  }
  ubu := ToU(ub, V, N, m, x, encodeByte)
  fmt.Printf("%s\n==> %s\n", ToCode(ub, "ub"), ToCode(ubu, "ubu"))
  
  hb, err := HexToBytes("349C5E19-E73A-4C71-AC05-3AD2E90A69C6")
  if err != nil {
    fmt.Printf("ERROR: failed to convert hb: %s!\n", err)
    return
  }
  hbu:= ToU(hb, V, N, m, x, encodeByte)
  fmt.Printf("%s\n== > %s\n", ToCode(hb, "hb"), ToCode(hbu, "hbu"))

  fmt.Println("Decode pbu...")
  pbv := ToU(pbu, N, V, m, y, decodeByte)
  Compare(pb, pbv)
  fmt.Println("Decode ubu...")
  ubv := ToU(ubu, N, V, m, y, decodeByte)
  Compare(ub, ubv)
  fmt.Println("Decode hbu...")
  hbv := ToU(hbu, N, V, m, y, decodeByte)
  Compare(hb, hbv)
}
