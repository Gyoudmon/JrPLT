#include <iostream> /* C++ 标准输入输出头文件 */

int main(int argc, char* argv[]) {
    // 定义长度为8的字符数组，使用初始化列表设置数组的全部元素
    char cs[8] = { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' };
    // END

    // 定义长度为8的字符串，隐含了结尾的'\0'，语法上看跟定义基础变量一样
    std::string str = "12345678";

    // 遍历数组需要用到循环，for 循环比较常见
    for (int n = 0; n < str.length(); n ++) {
        // 字符串可以像数组一样直接用“下标运算符[]”得到构成它的某个字符
        // 将字符数组cs里的内容复制给字符串str
        str[n] = cs[n];
    }
    // END

    std::cout << str << std::endl;

    return 0;
}
