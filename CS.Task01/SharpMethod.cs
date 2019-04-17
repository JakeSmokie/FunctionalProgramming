using System;
using System.Collections.Concurrent;
using System.Linq;
using System.Threading.Tasks;

namespace CS.Task01 {
    public static class SharpMethod {
        public static long GetResult(int n) {
            var maxMultiplier = (long) Math.Pow(10, n);

            for (var i = maxMultiplier - 1L; i >= 0; i--) {
                var p = MakePalindrome(i);

                if (CheckFactors(p)) {
                    return p;
                }
            }

            return -1;

            bool CheckFactors(long x) {
                var partitioner = Partitioner.Create(maxMultiplier / 10, maxMultiplier);
                var any = false;

                Parallel.ForEach(partitioner, range => {
                    var (a, b) = range;

                    for (var i = a; i < b; i++) {
                        if (x % i == 0 && x / i < maxMultiplier) {
                            any = true;
                        }

                        if (any) {
                            break;
                        }
                    }
                });

                return any;
            }

            long MakePalindrome(long x) {
                var s = x.ToString();
                return long.Parse(s + new string(s.Reverse().ToArray()));
            }
        }
    }
}