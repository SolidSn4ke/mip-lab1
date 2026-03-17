# Лабораторная работа 1

## Оглавление

- [Цель работы](#цель-работы)
- [Используемые формулы](#используемые-формулы)
- [Шаги алгоритма](#шаги-алгоритма)
- [Программа для расчета яркости](#программа-для-расчета-яркости)
- [Пример входных данных и результаты](#пример-входных-данных-и-результаты)
- [Вывод](#вывод)

## Цель работы

Изучить, как вычисляется яркость в точках на плоскости треугольника с учетом освещения от точечных источников света и диффузно отражательных свойств поверхности

## Используемые формулы

1. $I(RGB, \vec{s})=I_0(RGB)cos\theta$ - "цветная" **сила излучения** источника под углом к оси источника света, где $I_0(RGB)$ - "цветная" сила излучения источника света в направлении его оси $\vec{O}$, $||\vec{O}||=1$, $\theta$ - угол между направлением распространения света и осью источника света, $cos\theta$ - диаграмма излучения

2. $E(RGB, \vec{P_T})=\frac{I(RGB,\vec{s})cos\alpha}{R^2}$ - "цветная" **освещенность** точки, где $\alpha$ - угол между направлением света и нормалью к освещаемой поверхности, $R$ - расстояние от источника света до рассматриваемой точки

3. $\vec{P_T}=\vec{P_0}+(\frac{\vec{P_1}-\vec{P_0}}{||\vec{P_1}-\vec{P_0}||}\cdot x + \frac{\vec{P_2} - \vec{P_0}}{||\vec{P_2} - \vec{P_0}||} \cdot y)$ - **перевод локальных координат** точки в плоскости **в глобальные**, где $x$ и $y$ смещения по ребрам треугольника

4. $\vec{N}=\frac{(\vec{P_2}-\vec{P_0}) \times (\vec{P_1}-\vec{P_0})}{||(\vec{P_1}-\vec{P_0}) \times (\vec{P_2}-\vec{P_0})||}$ - вычисление **вектора нормали** плоскости треугольника через 3 точки

5. $\vec{s}=\vec{P_T}-\vec{P_L}, R^2=||\vec{s}||^2, cos\alpha=\frac{\vec{s}\cdot\vec{N}}{||\vec{s}||}, cos\theta=\frac{\vec{s}\cdot\vec{O}}{||\vec{s}||}$ - **вектор от точки плоскости до источника света**

6. $L(RGB, \vec{P_T}, \vec{v})=\frac{1}{\pi}\sum_{i \in lights}{(E_i(RGB, \vec{P_T}) \cdot f(RGB, \vec{P_T}, \vec{v}, \vec{s_i}))}$ - **яркость точки**, где $E_i(RGB, \vec{P_T})$ - освещённость от $i$-го источника света, $\vec{P_T}$ - координаты точки, $\vec{v}$ - напрвление наблюдателя, $\vec{s_i}$ - направление на источник света, $f(RGB, \vec{P_T}, \vec{v}, \vec{s_i})$ - двунаправленная функция отражения (BRDF), $K(RGB)$ - "цвет" поверхности

7. $f(RGB, \vec{P_T}, \vec{v}, \vec{s_i}) = K(RGB)(k_d + k_s(\vec{h} \cdot \vec{N})^{k_e})$ - **двунаправленная функция отражения**, где $k_d$ - коэффициент диффузного отраженияповерхности, $k_s$ - коэффициент зеркального отражения поверхности, $k_e$ - коэффициент, определяющий ширину блика, $\vec{h}$ - средний вектор между направлениями освещения и наблюдения, $\vec{N} - нормаль к поверхности в точке освещения$

8. $\vec{h}=\frac{\vec{v}+\vec{s}}{||\vec{v}+\vec{s}||}$ - **средний вектор**

## Шаги алгоритма

1. **Считывание параметорв из файла**

2. **Расчет освещенности**.

Сначала вызывается функция `calcIlluminance`, её задача посчитать все необходимые для расчета освещенности данные и передать их функции `colorIlluminance`

```haskell
calcIlluminance :: LightSource -> Triangle -> (Double, Double) -> Illuminance
calcIlluminance ls t (x', y') = colorIlluminance ls pT n
  where
    Triangle p_0 p_1 p_2 = t
    pT = localToGlobal x' y' p_0 p_1 p_2
    n = normalVec p_0 p_1 p_2
```

`calcIlluminance` вычисляет точку на плоскости `pT`, вектор нормали `n` и передает их функции `colorIlluminance`

```haskell
colorIlluminance :: LightSource -> Point -> Point -> Illuminance
colorIlluminance ls pT n = if z pL <= 0 then Illuminance $ RGB 0 0 0 else Illuminance $ mulVal rgb' $ cosA / rSqr
  where
    LightSource _ pL _ = ls
    Intensity rgb' = colorIntensity ls pT
    s = pL - pT
    cosA = normalize s `dot` normalize n
    rSqr = norm s ** 2
```

Для расчета освещенности используется соответствующая функция из раздела [Используемые формулы](#используемые-формулы)

3. **Расчет яркости**

Аналогично расчету освещенности, сначала вызывается функция `calcBrightness`, которая вычисляет все необходимые для расчета яркости параметры

```haskell
calcBrightness :: [LightSource] -> Triangle -> Point -> Surface -> (Double, Double) -> Brightness
calcBrightness ls t vP s (x', y') = if z vP <= 0 then Brightness $ RGB 0 0 0 else colorBrightness ls pT n v kRGB kD kS kE
  where
    Triangle p_0 p_1 p_2 = t
    Surface kRGB kD kS kE = s
    v = pT - vP
    pT = localToGlobal x' y' p_0 p_1 p_2
    n = normalVec p_0 p_1 p_2
```

`calcBrightness` вычисляет вектор `v` от точки наблюдателя к точке на плоскости, точку на плоскости `pT` и нормаль `n`. Далее вызывается функция `colorBrightness`

```haskell
colorBrightness :: [LightSource] -> Point -> Point -> Point -> RGB -> Double -> Double -> Double -> Brightness
colorBrightness ls pT n v kRGB kD kS kE = Brightness $ foldl helper (RGB 0 0 0) ls `mulVal` (1 / pi)
  where
    helper rgb' ls' =
        let
            (LightSource _ pL _) = ls'
            s = pT - pL
            (Illuminance eRgb) = colorIlluminance ls' pT n
         in
            rgb' `add` (eRgb `RGB.mul` brdf kRGB n v s kD kS kE)
```

Для расчета яркости используется соответствующая функция из раздела [Используемые формулы](#используемые-формулы)

## Программа для расчета яркости

[Листинг программы](https://github.com/SolidSn4ke/mip-lab1)

## Пример входных данных и результаты

Пример входных данных:

```json
{
    "lights": [
        {
            "i0": {
                "rgb": {"r": 100, "g": 10, "b": 0}
            },
            "o": {"x": 0, "y": 0, "z": -1},
            "p": {"x": 0, "y": 0, "z": 5}
        },
        {
            "i0": {
                "rgb": {"r": 0, "g": 0, "b": 100}
            },
            "o": {"x": 0, "y": 0, "z": -1},
            "p": {"x": 0, "y": 2, "z": 5}
        }
    ],
    "triangle": {
        "p0": {"x": 0, "y": 0, "z": 0},
        "p1": {"x": 0, "y": 5, "z": 0},
        "p2": {"x": 5, "y": 0, "z": 0}
    },
    "viewer": {"x": 1, "y": 1, "z": 5},
    "surface": {
        "k": {"r": 1, "b": 1, "g": 1},
        "kd": 0.8,
        "ks": 0.5,
        "ke": 100
    },
    "points": [
        [0, 0], [0.25, 0.25], [0.5, 0.5], [0.75, 0.75], [1, 1]
    ]
}
```

Результаты:

![Результаты](./image.png)

## Вывод

В данной работе был изучен и реализован алгоритм вычисления яркости точки на плоскости с учетом освещенности от нескольких источников света. Используя заданные входные данные, программа корректно вычисляет значения освещенности и яркости для различных точек плоскости, учитывая направленность света и свойства поверхности
