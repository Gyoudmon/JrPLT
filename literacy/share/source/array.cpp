#include <iostream> /* C++ 标准输入输出头文件 */

int main(int argc, char* argv[]) {
    /* 定义数组，必须使用常量指定数组的长度 */
    bool booleans[0];  // 设 0-长度 布尔数组 booleans
    double flonums[8]; // 设 8-长度 浮点数数组 flonums
    /* END */

    /* 定义数组, 其内容由右边的初始化列表赋予 */
    // 常规操作，8-长度 整数数组
    int integers[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
    // 因为数组的长度可以从初始化列表中推理出来，此时声明数组的时候可以省略长度信息
    bool logics[] = { true, false };
    // 初始化列表所含元素的数量可以比数组长度少，少了的部分自动补零
    float singles[8] = { 1.0F, 2.0F, 3.0F };
    // 因此，声明并初始化全0长整数数组可以简写成
    long long_integers[64] = {};
    /* END */

    /* 遍历数组需要用到循环，for 循环比较常见 */ 
    for (int n = 0; n < 8; n ++) {
        // 使用整数数组里的内容初始化浮点数数组
        flonums[n] = integers[n];
    }
    // END

    /* 活用 sizeof 可灵活得到数组的长度信息 */
    std::cout << sizeof(booleans) / sizeof(bool) << std::endl;
    std::cout << sizeof(flonums) / sizeof(double) << std::endl;
    std::cout << sizeof(logics) / sizeof(bool) << std::endl;
    std::cout << sizeof(singles) / sizeof(float) << std::endl;
    std::cout << sizeof(long_integers) / sizeof(long) << std::endl;
    // END

    /* 范围for，无需知道数组长度，直接得到每一个元素 */
    for (float fl : singles) {
        std::cout << fl << std::endl;
    }
    // END

    return 0;
}
