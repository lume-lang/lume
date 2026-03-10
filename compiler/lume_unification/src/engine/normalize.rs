use crate::engine::*;

#[tracing::instrument(level = "TRACE", skip_all, fields(%type_variable), err(Debug))]
pub(crate) fn normalize_equality_constraints<'ty, C: Context, I: Iterator<Item = (&'ty C::Ty, &'ty C::Ty)>>(
    ctx: &C,
    type_variable: TypeVar<C>,
    constraints: I,
) -> std::result::Result<C::Ty, Error<C>>
where
    <C as Context>::Ty: 'ty,
{
    let canonical_type_binding = ctx.canonical_of(type_variable).unwrap();
    let mut normalized_type: Option<C::Ty> = None;

    for (lhs, rhs) in constraints {
        let (normalized_lhs, normalized_rhs) = normalize_constraint_types(ctx, canonical_type_binding, lhs, rhs)?;

        tracing::trace!(
            type_var = %type_variable,
            lhs = %ctx.name_of_type(lhs).unwrap(),
            rhs = %ctx.name_of_type(rhs).unwrap(),
            normalized_lhs = %ctx.name_of_type(&normalized_lhs).unwrap(),
            normalized_rhs = %ctx.name_of_type(&normalized_rhs).unwrap(),
            "normalized_eq"
        );

        // Use the normalized type which does *not* contain the type variable itself,
        // since that wouldn't be very useful to the type checker.
        let resolved_type = if normalized_lhs.id() == canonical_type_binding {
            normalized_rhs
        } else {
            normalized_lhs
        };

        tracing::trace!(
            type_variable = %type_variable,
            subst = %ctx.name_of_type(&resolved_type).unwrap(),
            "substitute_type_variable"
        );

        match normalized_type.as_ref() {
            Some(entry) => {
                // If there's more than a single equality constraint, we have to check
                // whether they resolve to the same type. Otherwise, we must raise errors.
                if entry != &resolved_type {
                    return Err(Error::Mismatch {
                        lhs: entry.to_owned(),
                        rhs: resolved_type,
                    });
                }
            }
            None => {
                normalized_type = Some(resolved_type);
            }
        }
    }

    normalized_type.ok_or(Error::Unsolved(type_variable))
}

#[tracing::instrument(
    level = "TRACE",
    skip_all,
    fields(
        lhs = %ctx.name_of_type(lhs).unwrap(),
        rhs = %ctx.name_of_type(rhs).unwrap(),
    ),
    err(Debug)
)]
pub(crate) fn normalize_constraint_types<C: Context>(
    ctx: &C,
    target: C::ID,
    lhs: &C::Ty,
    rhs: &C::Ty,
) -> std::result::Result<(C::Ty, C::Ty), Error<C>> {
    // If either of the items in the set are the target, we send them back.
    if lhs.id() == target || rhs.id() == target {
        return Ok((lhs.to_owned(), rhs.to_owned()));
    }

    tracing::trace!(target = ?target, ?lhs, ?rhs);

    // If the two types being normalized don't refer to the type parent type, we
    // cannot normalize them. For example, image a set of constraints like this:
    // ```
    // U = [Option<?T> = Option<String>]
    // ```
    // can be normalized, since they both refer to the same containing type,
    // `Option`.
    //
    // Contrarily, this example cannot be normalized since they do not
    // refer to the same containing type:
    // ```
    // U = [Array<?T> = Option<String>]
    // ```
    if lhs.id() == rhs.id() {
        for (bound_lhs, bound_rhs) in lhs.bound_types().iter().zip(rhs.bound_types().iter()) {
            if !bound_lhs.contains(target) && !bound_rhs.contains(target) {
                continue;
            }

            return normalize_constraint_types(ctx, target, bound_lhs, bound_rhs);
        }
    }

    Err(Error::Mismatch {
        lhs: lhs.to_owned(),
        rhs: rhs.to_owned(),
    })
}
