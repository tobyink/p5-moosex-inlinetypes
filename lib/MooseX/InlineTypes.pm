package MooseX::InlineTypes;

use 5.008;
use strict;
use warnings;

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.001';

use Moose::Util::TypeConstraints ();
use Sub::Exporter -setup => {
	exports => [qw/ InlineTypes /],
	groups  => {
		default => [qw/ InlineTypes /],
	},
};

# Some mini helper subs
# 
my $WRAP = sub{ my $sub = shift; sub { local $_ = $_[0]; $sub->(@_) } };
my $FTC  = \&Moose::Util::TypeConstraints::find_type_constraint;

use constant do
{
	package MooseX::InlineTypes::Trait::Attribute;
	
	use Moose::Role;
	use Scalar::Does -constants;
	
	has isa_code => (
		is     => 'ro',
		isa    => 'CodeRef',
	);
	
	has coerce_array => (
		is     => 'ro',
		isa    => 'ArrayRef',
	);
	
	around _process_options => sub
	{
		my $orig = shift;
		my $meta = shift;
		my ($name, $options) = @_;
		
		if (does $options->{isa}, CODE)
		{
			$meta->_process_isa_code(@_);
			$meta->_make_isa(@_);
		}
		
		if (ref $options->{coerce})
		{
			$meta->_process_coerce_array(@_);
			$meta->_make_coerce(@_);
		}
		
		return $meta->$orig(@_);
	};
	
	sub _process_isa_code
	{
		my $meta = shift;
		my ($name, $options) = @_;
		
		$options->{isa_code} = delete $options->{isa};
	}
	
	sub _make_isa
	{
		my $meta = shift;
		my ($name, $options) = @_;
		
		if ($options->{associated_metaclass})
		{
			$name = $options->{associated_metaclass}->name . "::$name";
		}
		
		$options->{isa} = "Moose::Meta::TypeConstraint"->new(
			name       => "__INLINE__[$name]",
			parent     => $FTC->("Item"),
			constraint => $WRAP->( $options->{isa_code} ),
		);
	}
	
	sub _process_coerce_array
	{
		my $meta = shift;
		my ($name, $options) = @_;
		
		my $c = delete $options->{coerce};
		
		my @map;
		if (does $c, ARRAY)
		{
			my $idx;
			@map = map { ($idx++%2) ? $WRAP->($_) : $_ } @$c;
		}
		elsif (does $c, HASH)
		{
			# sort is a fairly arbitrary order, but at least it's
			# consistent. We prefer an ARRAY!
			# 
			for my $k (sort keys %$c)
			{
				push @map, $k => $WRAP->( $c->{$k} );
			}
		}
		elsif (does $c, CODE)
		{
			@map = (Item => $WRAP->($c));
		}
		else
		{
			confess "Unknown reference '$c' provided as coercion, confused";
		}
		
		$options->{coerce_array} = \@map;
	}
	
	sub _make_coerce
	{
		my $meta = shift;
		my ($name, $options) = @_;
		
		if ($options->{associated_metaclass})
		{
			$name = $options->{associated_metaclass}->name . "::$name";
		}
		
		# Got an inline coercion, but no inline type constraint. Don't want
		# to add coercions to Moose built-in type constraints, so clone the
		# current type constraint!
		# 
		if (not $options->{isa_code})
		{
			my $orig = $FTC->($options->{isa} || 'Item');
			$options->{isa} = Moose::Meta::TypeConstraint->new(
				name   => "__INLINE__[$name]",
				parent => $orig,
			);
		}
		
		my $coercions = "Moose::Meta::TypeCoercion"->new(
			type_constraint => $options->{isa},
		);
		$coercions->add_type_coercions(@{$options->{coerce_array}});
		$options->{isa}->coercion($coercions);
		$options->{coerce} = 1;
	}
	
	InlineTypes => __PACKAGE__;
};

1;

__END__

=head1 NAME

MooseX::InlineTypes - declare type constraints and coercions inline with coderefs

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=MooseX-InlineTypes>.

=head1 SEE ALSO

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2013 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.


=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

