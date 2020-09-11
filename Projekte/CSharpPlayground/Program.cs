using System;
using System.Collections.Generic;
using System.Linq;

namespace CSharpPlayground
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine(
                String.Join(",",
                    Wechsle(538)
                        .Select(x => x.ToString())));
                    
            
            // 1, 2, Fizz, 4, Buzz, Fizz, .... , 14, FizzBuzz
            // ist Zahl durch 3 Teilbar -> Fizz
            //                5 Teilbar -> Buzz
            //                3&5 Teilbar -> FizzBuzz
            //                sonst -> Zahl
            // String.Join(", ", ...)
            var zahlen = new List<string>();
            for (var n = 1; n <= 15; n++)
            {
                zahlen.Add(FizzBuzz(n));    
            }

            Console.WriteLine(String.Join(", ", zahlen));
        }

        static IEnumerable<string> Generiere(int n)
        {
            for (var i = 1; i <= n; i++)
                yield return FizzBuzz(i);
        }

        static IEnumerable<string> Generiere2(int n)
        {
            return Enumerable
                .Range(1, n)
                .Select(FizzBuzz);
        }
        
        static string FizzBuzz(int zahl)
        {
            if (zahl % 15 == 0) return "FizzBuzz";
            if (zahl % 5 == 0) return "Buzz";
            if (zahl % 3 == 0) return "Fizz";
            return zahl.ToString();
        }
       
        // 200, 100, 50, 20, 10, 5, 2, 1
        // 173 -> 100, 50, 20, 2, 1

        static IEnumerable<int> Wechsle(int betrag)
        {
            var muenzen = new int[] {200, 100, 50, 20, 10, 5, 2, 1};
            var rueckgabe = new List<int>();
            var index = 0;
            var restbetrag = betrag;
            while (restbetrag > 0)
            {
                var muenze = muenzen[index];
                if (muenze <= restbetrag)
                {
                    rueckgabe.Add(muenze);
                    restbetrag -= muenze;
                }
                else
                    index++;
            }

            return rueckgabe;
        }
        
    }
}