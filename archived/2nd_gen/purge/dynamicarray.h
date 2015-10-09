#ifndef DYNAMIC_ARRAY_H 
#define DYNAMIC_ARRAY_H 

template <class T> 
class DynamicArray {

  public:
    DynamicArray();
    DynamicArray(int initSize);
    DynamicArray(const DynamicArray &original);
    ~DynamicArray();

    void insert(int position, T value); 
    T get(int position);  

    int getSize();
    void resize(int newSize);

    int &operator[](int index);
    bool operator==(DynamicArray);
    bool operator!=(DynamicArray);

  private:
    int size;  
    T *array;

};

template <class T>
DynamicArray<T>::DynamicArray() {
  DynamicArray::DynamicArray(5);
}

template <class T>
DynamicArray<T>::DynamicArray(int initSize) {
  array = new T[initSize];
  for (int i = 0; i < size; i++) {
    array[i] = 0; // Fill with zeroes
  }
}

template <class T>
DynamicArray<T>::DynamicArray(const DynamicArray<T> &original) {
  size  = original.size;
  array = new T[size];
  for (int i = 0; i < size; i++) {
    array[i] = original.array[i];
  }
}

template <class T>
DynamicArray<T>::~DynamicArray() {
  delete[] array;
}

template <class T>
void DynamicArray<T>::insert(int position, T value) {
  array[position] = value;
}

template <class T>
T DynamicArray<T>::get(int position) {
  return array[position];
}

template <class T>
int DynamicArray<T>::getSize() {
  return size;
}

template <class T>
void DynamicArray<T>::resize(int newSize) {
  int *temp;
  temp = new T[newSize];
  for (int i = 0; i < (newSize); i++) {
    temp[i] = array[i];
  }
  delete[] array;
  array = temp;
  size = newSize;
}

template <class T>
bool DynamicArray<T>::operator==(DynamicArray a)  {
  if (a.size != size) return false;

  for (int i = 0; i < (a.size); i++) {
    if (a[i] != array[i]) return false;
  }
}

template <class T>
bool DynamicArray<T>::operator!=(DynamicArray a)  {
  if (a.size != size) return true;

  for (int i = 0; i < (a.size); i++) {
    if (a[i] != array[i]) return true;
  }
}

template <class T>
int &DynamicArray<T>::operator[](int index) {
  if ((index - 1) > size) {
    resize(index + 1);
  }
  return array[index]; // returned as a reference
}

#endif // DYNAMIC_ARRAY_H
