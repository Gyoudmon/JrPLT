#include <iostream> /* C++ 标准输入输出头文件 */

int main(int argc, char* argv[]) {
    /* 定义数组，必须使用常量指定数组的长度 */
    bool booleans[0];  // 设 0-长度 布尔数组 booleans
    double flonums[8]; // 设 8-长度 浮点数数组 flonums
    /* END */

    /* 定义 8-长度 整数数组 integers, 其内容由右边的初始化列表赋予 */
    int integers[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
    /* END */

    /* 遍历数组需要用到循环，for 循环比较常见 */ 
    for (int n = 0; n < 8; n ++) {
        // 使用整数数组里的内容初始化浮点数数组
        flonums[n] = integers[n];
    }
    // END

    /* 活用 sizeof 可灵活得到数组的长度信息 */
    std::cout << sizeof(flonums) / sizeof(double) << std::endl;
    // END


    /* 范围for，无需知道数组长度，直接得到每一个元素 */
    for (double fl : flonums) {
        std::cout << fl << std::endl;
    }
    // END

    return 0;
}
