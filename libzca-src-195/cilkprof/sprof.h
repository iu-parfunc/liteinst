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


#ifndef _SPA_H_
#define _SPA_H_

#include <cstddef>
#include "log.h"
#include <stack>
#include <vector>

#define SPROF_MAX_LIST_SIZE 1024u
#define SPROF_MIN_TABLE_CAPACITY 1024u

/*
 * Span profile.
 */
template <typename T>
class sprof_t
{
public:
    /* Table storage for span data.  It is expensive to create and copy
       this, so it is only created after its copy costs have been
       ammortized over the number of insertions on the sprof. */
    typedef std::vector<unsigned int> table_t;

    sprof_t () :
        m_table(NULL),
        m_log()
    {
    }
    /*
     * Increment the value at the index specified.
     */
    void increment (T value)
    {
        // If it can be added directly to the table, do it.  Otherwise, add
        // it to the log and decide whether the log has become long enough
        // that it should be merged into the table.
        if ((NULL != m_table) && (value.get_index() < m_table->size())) {
            m_table->at(value.get_index()) += value.get_cost();
        } else {
            m_log.push_back(value);
            // Condense the structure, if it's provably cheap to do so.
            condense(false);
        }
    }
    /*
     * Move all of the data into the table portion of the structure.
     */
    void tabulate ()
    {
        // Force the condense.
        condense(true);
    }
    /*
     * Merge "that" sprof_t into "this" one.  The process will erase the
     * contents of "that" sprof_t.
     */
    void add_and_clear (sprof_t *that)
    {
        // Merge tables.
        if (that->m_table) {
            if (NULL == this->m_table) {
                // We don't have a table, but they do.  Take theirs.
                this->m_table = that->m_table;
                that->m_table = NULL;
            } else {
                // We both have tables...
                if (this->m_table->size() < that->m_table->size()) {
                    // Their table is larger.  Swap them so that we are
                    // always merging the smaller into the larger.
                    table_t *tmp = that->m_table;
                    that->m_table = this->m_table;
                    this->m_table = tmp;
                }
                // Combine both tables into our table.
                for (unsigned int i = that->m_table->size() - 1; i != 0; --i) {
                    this->m_table->at(i) += that->m_table->at(i);
                }
                recycle_table(that->m_table); // Zeroes out the table.
                that->m_table = NULL;
            }
        }

        // Merge lists.
        this->m_log.splice(that->m_log);
        this->condense(false);
    }
    /*
     * Perform an assignment operation and take the value of "that".  "that"
     * will be emptied in the process.
     */
    void assign_and_clear (sprof_t *that)
    {
        // Take "that" guy's table.
        if (this->m_table) {
            recycle_table(this->m_table); // Zeroes out the table.
        }
        this->m_table = that->m_table;
        that->m_table = NULL;

        // Take "that" guy's log.
        this->m_log.clear();
        this->m_log.splice(that->m_log);
        // Don't need to condense.  If "that" guy's log were large enough
        // to be condensed, "that" guy would have done it.
    }
    /*
     * Erase the contents of the sprof_t object.
     */
    void clear ()
    {
        if (this->m_table) {
            recycle_table(this->m_table); // Zeroes out the table.
            this->m_table = NULL;
        }
        this->m_log.clear();
    }
    unsigned int get (unsigned int i)
    {
        assert(this->m_table);
        assert(this->m_table->size() >= i);
        return this->m_table->at(i);
    }

    bool is_valid_index (unsigned int i)
    {
	if (NULL == m_table)
	    return false;
	return i < m_table->size();
    }

    ~sprof_t ()
    {
        if (this->m_table) {
            clear();
        }
    }

    size_t table_size()
    {
	if (m_table)
	    return m_table->size();
	else
	    return 0;
    }

    size_t table_capacity()
    {
	if (m_table)
	    return m_table->capacity();
	else
	    return 0;
    }

    size_t log_size()
    {
	return m_log.size();
    }

    size_t total_size()
    {
	size_t size;
	if (m_table)
	    size = m_table->size();
	else
	    size = 0;
	return m_log.size() + size;
    }

private:

    /*
     * Condense the structure into the table, if the cost of doing so has
     * been amortized.  Alternatively, the caller can "force" the process.
     * If the condense takes place, the log will be emptied.
     */
    void condense (bool force)
    {
	// If the log is short and we're not being forced to condense the list
	// into the table, we're done
	if ((! force) && (m_log.size() < SPROF_MAX_LIST_SIZE))
	    return;

	// The table needs to be at least big enough to hold the maximum index in the log
	// Don't forget that vector indicies are 0 based, while size is 1 based so we need
	// add 1
	unsigned int min_table_size = m_log.max_index() + 1;

	// If we haven't allocated a table yet, do so now
	if (NULL == m_table)
	{
	    m_table = get_table(min_table_size);
	}

	// Make sure the table will hold all of the indexes
	// (basic block numbers) for all of the entries from the log.
	if (m_table->size() < min_table_size)
	    m_table->resize(min_table_size, 0);

	// Move all of the data from the log into the table.
	for (typename log_t<T>::iterator iter = m_log.begin();
	     iter != m_log.end();
	     ++iter)
	{
	    T val = *iter;
	    m_table->at(val.get_index()) += val.get_cost();
	}

	// Empty the log.
	m_log.clear();
    }

    // The table is indexed by basic block number.
    table_t *m_table;

    // If the table is not big enough for an index, a log is kept of such
    // writes, until the overhead of growing the table has been ammortized.
    log_t<T> m_log;

    // Recycle functionality to preserve free'd tables.
    static typename std::stack<table_t*> table_stack;
    static table_t *get_table (unsigned int initial_table_size);
    static void recycle_table (table_t *t);
};

/***********************************************************************/
/*                            Static Members                           */
/***********************************************************************/

/* Stack of recycled tables. */
template <typename T>
std::stack<typename sprof_t<T>::table_t*> sprof_t<T>::table_stack =
    std::stack<typename sprof_t<T>::table_t*>();

/*
 * Get a table from the stack, make sure it is at least of size 'n', and
 * return it to the caller.
 */
template <typename T>
typename sprof_t<T>::table_t *sprof_t<T>::get_table (unsigned int initial_table_size)
{
    sprof_t<T>::table_t *t;

    if (table_stack.empty())
    {
	// If there are none in the look-aside list, create a new one with a
	// minimum capacity of 1024.  Since vectors will double the capacity
	// at need as they're resized, this should quickly get us past the
	// "resize alot" knee
	t = new sprof_t<T>::table_t();
	t->reserve(SPROF_MIN_TABLE_CAPACITY);
    }
    else
    {
	// Grab a table off of the look-aside list
	t = table_stack.top();
	table_stack.pop();
	t->resize(0);
    }

    // Set the table's size to match the request, filling all elements with "0"
    t->resize(initial_table_size, 0);

    return t;
}

/*
 * Store a table onto the table_stack after zeroing out its entries.
 */
template <typename T>
void sprof_t<T>::recycle_table (table_t *t)
{
    assert(t);
    table_stack.push(t);
}

/***********************************************************************/
/*                    Span Profile Value Structures                    */
/***********************************************************************/

struct sprof_single_t
{
    unsigned int index;
    sprof_single_t (unsigned int i) :
        index(i)
    {
    }
    unsigned int get_index () { return index; }
    unsigned int get_cost () { return 1; }
};

struct sprof_pair_t
{
    unsigned int index;
    unsigned int cost;
    sprof_pair_t (unsigned int value) :
        index(value), cost(1)
    {
    }
    sprof_pair_t (unsigned int i, unsigned int c) :
        index(i), cost(c)
    {
    }
    unsigned int get_index () { return index; }
    unsigned int get_cost () { return cost; }
};

#undef MIN_THRESHOLD

#endif /* defined _SPA_H_ */
