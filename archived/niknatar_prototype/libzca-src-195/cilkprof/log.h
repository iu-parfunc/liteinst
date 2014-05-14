/* -*- C++ -*- */

/*
  Copyright (C) 2012 Intel Corporation
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.
  * Neither the name of Intel Corporation nor the names of its
    contributors may be used to endorse or promote products derived
    from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _LOG_H_
#define _LOG_H_

#include <assert.h>
#include <list>
#include <vector>

/*
 * Wrapper for std::list that can determine the size() in constant time.
 * C++0x requires size() in constant time, but C++98 didn't require it,
 * and most implementations use linear time.
 */
template <typename T>
class log_t
{
public:
    typedef std::vector<T> uv;
    typedef std::list<uv> lov;

    /*
     * The iterator for log_t cannot be the iterator for
     * the std::list if we want to abstract it to iterate over the
     * elements of the std::vector as well.  So the class is defined
     * to iterate over both.
     */
    class iterator
    {
    public:
        iterator (typename lov::iterator iter,
                  unsigned int i) :
            list_iter(iter), index(i)
        {
        }

        bool operator == (iterator &that)
        {
            if (this->list_iter == that.list_iter &&
                this->index == that.index) {
                return true;
            }
            return false;
        }

        bool operator != (iterator that)
        {
            return ! (*this == that);
        }

        iterator &operator ++ ()
        {
            // warning: user should verify that ++ is a "safe" operation.
            if (++index >= (*list_iter).size()) {
                ++list_iter;
                index = 0;
            }
            return *this;
        }

        T operator * ()
        {
            return (*list_iter)[index];
        }

    private:
        /* Iterator over the outer list. */
        typename lov::iterator list_iter;

        /* Index into the vector in the current list element. */
        unsigned int index;
    };

    log_t () :
        l(),
        m_length(0),
	m_max_index(0)
    {
    }

    ~log_t ()
    {
        list_clear(l);
    }

    void push_back (T val)
    {
        ++m_length;
        if (l.empty()) {
            list_add_element(l);
        }
        l.front().push_back(val);

	unsigned int i = val.get_index();
	if (i > m_max_index)
	    m_max_index = i;
    }

    std::size_t size ()
    {
	return m_length;
    }

    iterator begin ()
    {
        return iterator(l.begin(), 0);
    }

    unsigned int max_index()
    {
	return m_max_index;
    }

    iterator end ()
    {
        return iterator(l.end(), 0);
    }

    void clear ()
    {
        m_length = 0;
	m_max_index = 0;
        list_clear(l);
    }

    void splice (log_t &that)
    {
        this->m_length += that.m_length;
        that.m_length = 0;

        if (this->m_max_index < that.m_max_index)
	    this->m_max_index = that.m_max_index;
	that.m_max_index = 0;

        this->l.splice(this->l.end(), that.l);
    }

    /* Functions for caching free'd elements. */
    static void list_add_element (lov &l);
    static void list_clear (lov &l);

private:
    /* The list of basic blocks that have been executed. */
    lov l;

    /* The number of elements in the list. */
    std::size_t m_length;

    /* The maximum index in the list */
    unsigned int m_max_index;

    /* Recycled vectors. */
    static lov free_list;
};

/***********************************************************************/
/*                            Static Members                           */
/***********************************************************************/

/* List of recycled vectors. */
template <typename T>
typename log_t<T>::lov log_t<T>::free_list = log_t<T>::lov();

/*
 * Push back an element onto the list.
 */
template <typename T>
void log_t<T>::list_add_element (log_t<T>::lov &l)
{
    if (log_t<T>::free_list.empty()) {
        l.push_front(uv());
    } else {
        // Take an element off of the free list.
        typename log_t<T>::lov::iterator iter =
            log_t<T>::free_list.begin();
        (*iter).clear();
        ++iter;
        l.splice(l.end(),
                 log_t<T>::free_list,
                 log_t<T>::free_list.begin(),
                 iter);
    }
}

/*
 * Clear the list so that it is empty.
 */
template <typename T>
void log_t<T>::list_clear (log_t<T>::lov &l)
{
    // Recycle the vectors by putting them into the list of free vectors.
    log_t<T>::free_list.splice(log_t<T>::free_list.begin(), l);
}

#endif /* defined _LOG_H_ */
