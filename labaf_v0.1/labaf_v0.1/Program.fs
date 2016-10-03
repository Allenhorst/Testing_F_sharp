open System                                // Множество Мандельброта - простейший код
open System.Numerics
open System.Windows.Forms
open System.Drawing
open Microsoft.FSharp.Core.Operators
let rec nrpt f x = function           // рекурсивная функция для проверки сходимости
   | 0 -> x                                   // последовательности комплексных чисел
   | n -> nrpt f (f x) (n - 1)
//let mandelf c z = z * z + c          // собственно последовательность комплексных чисел
let mandelf c z = (c / cos(z))*(c / cos (z))
let ismandel_first c = (Complex.Abs (nrpt (mandelf c) Complex.Zero 30) < 0.5 ) && ( Complex.Abs (nrpt (mandelf c) Complex.Zero 30) > 0.05 )     // сходится по первому пределу? 15 0.5
let ismandel_second c = ( Complex.Abs (nrpt (mandelf c) Complex.Zero 30)  < 0.05 ) && ( Complex.Abs (nrpt (mandelf c) Complex.Zero 30) > 0.01 )     // сходится по второму пределу? 0.5 0.1
let ismandel_third c = Complex.Abs (nrpt (mandelf c) Complex.Zero 30) < 0.01       // сходится по третьему пределу? 0.1
//let ismandel_fourth c = Complex.Abs (nrpt (mandelf c) Complex.Zero 30) < 0.1       // сходится по 4му пределу?
let colmandel_first = function true -> Color.BlueViolet | _ -> Color.AntiqueWhite     // выбор цвета для отрисовки 1
let colmandel_second = function true -> Color.Coral | _ -> Color.AntiqueWhite     // выбор цвета для отрисовки 1
let colmandel_third = function true -> Color.LawnGreen | _ -> Color.AntiqueWhite     // выбор цвета для отрисовки 1

let scale (x,y) (u,v) n = float(n - u) / float(v - u) * (y - x) + x                            // масштабирование
let form =
   let image = new Bitmap(600, 600)                                                     // битовая карта для отрисовки
   let lscale = scale (-1.2, 1.2) (0, image.Height-1)                                           // масштабируем
   let rec filler = function                                                                                // и рисуем ...
   | -1 , _ -> ()
   | y , -1 -> filler (y - 1, (image.Width - 1))
   | y , x  -> let t = Complex(lscale y, lscale x)
      
               if ( t|> ismandel_third  ) then image.SetPixel ( y, x, t |> ismandel_third |> colmandel_third )                 // конвейер
               if ( t|> ismandel_second  ) then image.SetPixel ( y, x, t |> ismandel_second |> colmandel_second )               // конвейер
               if ( t|> ismandel_first  ) then image.SetPixel ( y, x, t |> ismandel_first |> colmandel_first )                // конвейер
               
               filler (y, x - 1)
   filler (image.Height - 1, image.Width - 1)
   let temp = new Form(Width = 600, Height = 600)                               // сделаем окошко ...
   temp.Paint.Add(fun e -> e.Graphics.DrawImage (image, 0, 0))           // битовую карту - в окошко
   temp                                                                                               // всё вычислим ...
do Application.Run(form)                                                                    // старт приложения