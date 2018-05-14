#include <iostream>

// identity function
template <class T>
T id(T x)
{
    return x;
}

// unit function
template <class T>
void unit(T) {}

int main()
{
    std::cout << "Hello World: " << id(3) << std::endl;
    std::cout << "Indentity string: " << id("myself") << std::endl;
    unit("nothing");
    return 0;
}

int fac(int n) {
    int acc = 1;
    for (int i = 1; i <= n; i++) {
        acc = i * acc;
    }
    return acc;
}