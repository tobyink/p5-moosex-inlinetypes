package MooseX::InlineTypes;

use 5.008;
use strict;
use warnings;

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.001';

use Moose::Util::TypeConstraints ();
use Sub::Exporter ();
use Sub::Install qw( install_sub );

my $EXPORT;
sub import
{
	$EXPORT ||= Sub::Exporter::build_exporter {
		exports => [qw/ InlineTypes /],
		groups  => {
			default => [qw/ InlineTypes /],
		},
	};
	
	if (grep { !ref and /^-global$/ } @_)
	{
		@_ = grep { ref or !/^-global$/ } @_;
		$_[0]->_alter_has(scalar caller);
	}
	
	goto $EXPORT;
}

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
	
	before _process_options => sub
	{
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
		
		if ($options->{definition_context}{package})
		{
			$name = $options->{definition_context}{package} . "::$name";
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
		elsif (0 and does $c, HASH)  # commented out!
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
		
		if ($options->{definition_context}{package})
		{
			$name = $options->{definition_context}{package} . "::$name";
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

sub _alter_has
{
	my ($class, $caller) = @_;
	my $orig = $caller->can('has')
		or Carp::croak("Cannot find 'has' function to mess with, stuck");
	
	# Prevent warning about "has" being redefined!
	# 
	require namespace::clean;
	namespace::clean->clean_subroutines($caller, 'has');
	
	install_sub {
		into    => $caller,
		as      => 'has',
		code    => sub {
			my ($name, %args) = @_;
			push @{ $args{traits} ||= [] }, InlineTypes;
			@_ = ($name, %args) and goto $orig;
		},
	};
}

1;

__END__

=head1 NAME

MooseX::InlineTypes - declare type constraints and coercions inline with coderefs

=head1 SYNOPSIS

   use v5.14;
   
   package Document {
      use Moose;
      use MooseX::InlineTypes;
      has heading => (
         traits  => [ InlineTypes ],
         is      => "ro",
         isa     => sub { !ref($_) and length($_) < 64 },
         coerce  => sub { sprintf("%s...", substr($_, 0, 60)) },
      );
   }

=head1 DESCRIPTION

This module provides an attribute trait that allows you to declare L<Moose>
type constraints and coercions inline using coderefs, a bit like L<Moo>,
but not quite.

=head2 C<< isa => CODEREF >>

This is a coderef which returns true if the value passes the type constraint
and false otherwise.

=head2 C<< coerce => CODEREF >>

This is a coderef which takes the uncoerced value and returns the coerced
value.

=head2 C<< coerce => ARRAYREF >>

This allows you to specify several different coercions from different types:

   isa    => "ArrayRef",
   coerce => [
      Str     => sub { split /\s+/, $_ },
      HashRef => sub { sort values %$_ },
      CodeRef => sub { my @r = $_->(); \@r },
   ],

The order of coercions is significant. For example, given the following the
C<Int> coercion is never attempted, because C<Any> is tried first!

   coerce => [
      Any     => sub { ... },
      Int     => sub { ... },
   ],

Note that C<< coerce => CODEREF >> is really just a shorthand for
C<< coerce => [ Item => CODEREF ] >>.

Attributes declared with the MooseX::InlineTypes trait do still support the
"normal" Moose C<isa> and C<coerce> options, though it should be noted that
C<< isa=>CODE, coerce=>1 >> makes no sense and Moose will give you a
massive warning!

=head1 EXPORT

=over

=item C<< InlineTypes >>

This is an exported constant so that you can write:

   traits  => [ InlineTypes ],

Instead of the more long-winded:

   traits  => [ "MooseX::InlineTypes::Trait::Attribute" ],

=item C<< -global >>

If you do this:

   use MooseX::InlineTypes -global;

Then the InlineTypes trait will be applied automatically to I<all> your
attributes. (Actually only to those attributes declared using C<has>.
Attributes added via Moose meta calls will be unaffected.)

Don't worry; it's not really global. It's just for the caller.

=back

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=MooseX-InlineTypes>.

=head1 SEE ALSO

L<Moose>, L<Moo>.

=head2 Usage with MooseX::Types

Here's the example from the SYNPOSIS rewritten using L<MooseX::Types>:

   use v5.14;
   
   package Document {
      use Moose;
      use MooseX::InlineTypes -global;
      use MooseX::Types qw( Str is_Str );
      has heading => (
         is      => "ro",
         isa     => sub { is_Str($_) and length($_) < 64 },
         coerce  => [
            Str, sub { sprintf("%s...", substr($_, 0, 60)) },
         ]
      );
   }

Note that MooseX::Types exports C<< is_X >> functions for each type which
can be useful inside the C<< isa >> coderefs.

With coercion arrayrefs, beware the magic quoting power of the fat comma!

=head1 HISTORY

This was originally a patch for L<MooseX::AttributeShortcuts> until
Matt S Trout (cpan:MSTROUT) convinced me to rewrite it independently
of that.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2012-2013 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

