#ifndef F95_SUPPORT_H
#define F95_SUPPORT_H

#define boolean_type_node		c_global_trees[CTI_BOOLEAN_TYPE]
#define boolean_true_node		c_global_trees[CTI_BOOLEAN_TRUE]
#define boolean_false_node		c_global_trees[CTI_BOOLEAN_FALSE]

tree convert_and_check PARAMS((tree, tree));
void overflow_warning PARAMS((tree));
void unsigned_conversion_warning PARAMS((tree, tree));
int g95_mark_addressable PARAMS((tree));

void g95_init_c_decl_hacks(void);

#endif
